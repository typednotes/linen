"""Exercise one precompiled Lean worker through a local package and HTTPS."""

import argparse
import hashlib
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import json
import os
from pathlib import Path
import ssl
import subprocess
from tempfile import TemporaryDirectory
from threading import Thread


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("binary", type=Path)
    binary = parser.parse_args().binary.resolve(strict=True)
    worker = binary.with_name("cell-worker-fixture").resolve(strict=True)

    with TemporaryDirectory(prefix="linen-cell-") as directory:
        root = Path(directory)
        key_home = root / "keys"
        key_home.mkdir(mode=0o700)
        environment = dict(os.environ, GNUPGHOME=str(key_home))

        def run(*args, input=None, check=True):
            return subprocess.run(
                args, env=environment, input=input, capture_output=True, check=check, timeout=40
            )

        try:
            run("gpg", "--batch", "--passphrase", "", "--quick-generate-key",
                "Linen test compiler <compiler@cell.example>", "ed25519", "sign", "1d")
            listing = run("gpg", "--batch", "--with-colons", "--list-secret-keys").stdout
            fingerprint = next(line.split(":")[9] for line in listing.decode().splitlines()
                               if line.startswith("fpr:"))
            keyring = root / "compiler.gpg"
            keyring.write_bytes(run("gpg", "--batch", "--export", fingerprint).stdout)

            pin = run(binary, "publish", root, worker, fingerprint).stdout.decode().strip()
            run(binary, "verify", root, keyring)
            run(binary, "local", root, keyring, pin)

            def reject_local(expected, selected_pin=pin):
                result = run(binary, "local", root, keyring, selected_pin, check=False)
                assert result.returncode and expected in result.stderr, result.stderr

            signature = root / "manifest.asc"
            original_signature = signature.read_bytes()
            signature.write_bytes(b"invalid detached signature")
            reject_local(b"cell compiler signature verification failed")
            signature.write_bytes(original_signature)

            artifact = root / "worker"
            original_artifact = artifact.read_bytes()
            artifact.write_bytes(b"X" + original_artifact[1:])
            reject_local(b"cell worker does not match its manifest")
            artifact.write_bytes(original_artifact)
            artifact.chmod(0o700)

            record = root / "manifest.json"
            original_record = record.read_bytes()
            record.write_bytes(original_record.rstrip(b"\n"))
            assert run(binary, "verify", root, keyring, check=False).returncode
            record.write_bytes(original_record)
            incompatible = json.loads(original_record)
            incompatible["runtime"]["runtimeAbi"] = "another-abi"
            encoded = json.dumps(incompatible).encode()
            record.write_bytes(encoded)
            reject_local(b"incompatible cell runtime or codecs", hashlib.sha256(encoded).hexdigest())
            record.write_bytes(original_record)

            cert, key = root / "server.crt", root / "server.key"
            run("openssl", "req", "-x509", "-newkey", "rsa:2048", "-nodes", "-days", "1",
                "-subj", "/CN=localhost", "-addext", "subjectAltName=DNS:localhost",
                "-keyout", key, "-out", cert)
            environment["CURL_CA_BUNDLE"] = str(cert)

            class CellServer(BaseHTTPRequestHandler):
                def log_message(self, *_args):
                    pass

                def send_bytes(self, body):
                    self.send_response(200)
                    self.send_header("Content-Length", str(len(body)))
                    self.end_headers()
                    self.wfile.write(body)

                def do_GET(self):
                    names = {"/manifest.json", "/manifest.asc", "/worker"}
                    if self.path in names:
                        self.send_bytes((root / self.path[1:]).read_bytes())
                    else:
                        self.send_error(404)

                def do_POST(self):
                    if self.path != "/call":
                        self.send_error(404)
                        return
                    request = self.rfile.read(int(self.headers["Content-Length"]))
                    response = run(artifact, "respond", pin, input=request + b"\n")
                    self.send_bytes(response.stdout)

            with ThreadingHTTPServer(("127.0.0.1", 0), CellServer) as server:
                tls = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
                tls.load_cert_chain(cert, keyfile=key)
                server.socket = tls.wrap_socket(server.socket, server_side=True)
                thread = Thread(target=server.serve_forever)
                thread.start()
                try:
                    url = f"https://localhost:{server.server_port}"
                    run(binary, "https", url, keyring, pin)
                    wrong_pin = "0" * 64
                    assert run(binary, "https", url, keyring, wrong_pin, check=False).returncode
                    request = {"jsonrpc": "2.0", "id": 0, "method": "call",
                               "params": [{"version": 1, "pin": wrong_pin}, 2, 3]}
                    reply = run(artifact, "respond", pin, input=json.dumps(request).encode() + b"\n")
                    assert json.loads(reply.stdout)["error"]["message"] == "cell pin or wire version mismatch"
                finally:
                    server.shutdown()
                    thread.join()
            print("PASS: one compiled addition cell, repeated local calls, HTTPS, and tamper rejection")
        finally:
            run("gpgconf", "--kill", "gpg-agent", check=False)


if __name__ == "__main__":
    main()
