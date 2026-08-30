#!/usr/bin/env python3
import http.server
import socketserver
import json
import os
import subprocess
import urllib.parse
import urllib.request

CBIO_API = "https://www.cbioportal.org/api"

PORT = 3000
DIRECTORY = "public"
ROOT_DIR = os.path.dirname(os.path.abspath(__file__))

class Handler(http.server.SimpleHTTPRequestHandler):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, directory=DIRECTORY, **kwargs)
        
    def do_GET(self):
        parsed_path = urllib.parse.urlparse(self.path)
        
        # API to list available files
        if parsed_path.path == '/api/files':
            files = [f for f in os.listdir(ROOT_DIR) if f.endswith('.csv') or f.endswith('.txt')]
            self.send_response(200)
            self.send_header('Content-type', 'application/json')
            self.end_headers()
            self.wfile.write(json.dumps({"files": files}).encode())
            return
            
        # API to fetch the plot
        elif parsed_path.path == '/api/results/km_plot.pdf':
            pdf_path = os.path.join(ROOT_DIR, "km_plot.pdf")
            if os.path.exists(pdf_path):
                self.send_response(200)
                self.send_header('Content-type', 'application/pdf')
                # Cache-control to prevent browser from keeping old plot
                self.send_header('Cache-Control', 'no-store, no-cache, must-revalidate, max-age=0')
                self.end_headers()
                with open(pdf_path, 'rb') as f:
                    self.wfile.write(f.read())
            else:
                self.send_error(404, "File not found")
            return
            
        # API to fetch the text results
        elif parsed_path.path == '/api/results/survival_results.txt':
            txt_path = os.path.join(ROOT_DIR, "survival_results.txt")
            if os.path.exists(txt_path):
                self.send_response(200)
                self.send_header('Content-type', 'text/plain')
                self.send_header('Cache-Control', 'no-store, no-cache, must-revalidate, max-age=0')
                self.end_headers()
                with open(txt_path, 'rb') as f:
                    self.wfile.write(f.read())
            else:
                self.send_error(404, "File not found")
            return
            
        # Proxy: list all cBioPortal studies
        elif parsed_path.path == '/api/cbio/studies':
            try:
                req = urllib.request.Request(
                    f"{CBIO_API}/studies?projection=DETAILED&pageSize=10000&sort=studyId,asc",
                    headers={"Accept": "application/json"}
                )
                with urllib.request.urlopen(req, timeout=15) as resp:
                    raw = resp.read()
                self.send_response(200)
                self.send_header('Content-type', 'application/json')
                self.send_header('Access-Control-Allow-Origin', '*')
                self.send_header('Cache-Control', 'no-store, no-cache, must-revalidate, max-age=0')
                self.end_headers()
                self.wfile.write(raw)
            except Exception as e:
                self.send_response(502)
                self.send_header('Content-type', 'application/json')
                self.end_headers()
                self.wfile.write(json.dumps({"error": str(e)}).encode())
            return

        # Proxy: list molecular profiles for a study
        elif parsed_path.path.startswith('/api/cbio/profiles'):
            qs = urllib.parse.parse_qs(parsed_path.query)
            study = qs.get('study_id', [None])[0]
            if not study:
                self.send_error(400, "Missing study_id parameter")
                return
            try:
                url = f"{CBIO_API}/studies/{urllib.parse.quote(study)}/molecular-profiles"
                req = urllib.request.Request(url, headers={"Accept": "application/json"})
                with urllib.request.urlopen(req, timeout=10) as resp:
                    raw = resp.read()
                self.send_response(200)
                self.send_header('Content-type', 'application/json')
                self.send_header('Access-Control-Allow-Origin', '*')
                self.send_header('Cache-Control', 'no-store, no-cache, must-revalidate, max-age=0')
                self.end_headers()
                self.wfile.write(raw)
            except Exception as e:
                self.send_response(502)
                self.send_header('Content-type', 'application/json')
                self.end_headers()
                self.wfile.write(json.dumps({"error": str(e)}).encode())
            return

        # Serve static files from public directory
        return super().do_GET()

    def do_POST(self):
        parsed_path = urllib.parse.urlparse(self.path)
        
        if parsed_path.path == '/api/analyze':
            content_length = int(self.headers.get('Content-Length', 0))
            post_data = self.rfile.read(content_length) if content_length > 0 else b'{}'
            data = json.loads(post_data.decode('utf-8'))
            
            # Construct the command
            cmd = ["Rscript", "survival_analysis.R"]
            
            if data.get('download_cbio'):
                cmd.append("--download_cbio")
                if data.get('study_id'):
                    cmd.extend(["--study_id", data.get('study_id')])
                if data.get('profile_type'):
                    cmd.extend(["--profile_type", data.get('profile_type')])
            else:
                if data.get('input'):
                    cmd.extend(["-i", data.get('input')])
            if data.get('gene'):
                cmd.extend(["-g", data.get('gene')])
                
            if data.get('datatype'):
                cmd.extend(["--datatype", data.get('datatype')])

            if data.get('endpoint'):
                cmd.extend(["--endpoint", data.get('endpoint')])

            if data.get('split_method'):
                cmd.extend(["-m", data.get('split_method')])

            if data.get('cna_mode'):
                cmd.extend(["--cna_mode", data.get('cna_mode')])
            
            if data.get('cases'):
                # Write cases to a temporary file to avoid command-line length limits
                cases_file = os.path.join(ROOT_DIR, "cases_filter.txt")
                with open(cases_file, "w", encoding="utf-8") as f:
                    # Clean up quotes just in case
                    cleaned_cases = data.get('cases').replace('"', '').replace("'", "")
                    f.write(cleaned_cases)
                cmd.extend(["-c", cases_file])

                if data.get('id_col'):
                    cmd.extend(["-u", data.get('id_col')])
                
            # Run the command
            try:
                print(f"Running command: {' '.join(cmd)}")
                result = subprocess.run(cmd, cwd=ROOT_DIR, capture_output=True, text=True)
                
                if result.returncode == 0:
                    self.send_response(200)
                    self.send_header('Content-type', 'application/json')
                    self.end_headers()
                    self.wfile.write(json.dumps({
                        "status": "success", 
                        "message": "Analysis complete!",
                        "stdout": result.stdout
                    }).encode())
                else:
                    self.send_response(500)
                    self.send_header('Content-type', 'application/json')
                    self.end_headers()
                    self.wfile.write(json.dumps({
                        "status": "error", 
                        "message": "Analysis failed", 
                        "stderr": result.stderr,
                        "stdout": result.stdout
                    }).encode())
            except Exception as e:
                self.send_response(500)
                self.send_header('Content-type', 'application/json')
                self.end_headers()
                self.wfile.write(json.dumps({
                    "status": "error", 
                    "message": str(e)
                }).encode())
            return
            
        self.send_error(404, "Not Found")

# Setup server
socketserver.TCPServer.allow_reuse_address = True
with socketserver.TCPServer(("", PORT), Handler) as httpd:
    print(f"Serving at http://localhost:{PORT}")
    httpd.serve_forever()
