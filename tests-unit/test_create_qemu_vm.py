import os
import unittest

# Load the script dynamically since it doesn't have a .py extension
script_path = os.path.abspath(
    os.path.join(
        os.path.dirname(__file__),
        "../dotfiles-overlay/home/new-user/.local/bin/create-qemu-vm",
    )
)

# Read and execute the script in a dictionary to extract its functions
script_module = {}
with open(script_path, "r") as f:
    exec(f.read(), script_module)

generate_vm_filename = script_module["generate_vm_filename"]
parse_args = script_module["parse_args"]


class TestCreateVM(unittest.TestCase):

    def test_generate_vm_filename_standard(self):
        filename = generate_vm_filename("debian", "x86_64", "1G")
        self.assertEqual(filename, "debian.x86_64.1G.qcow2")

    def test_generate_vm_filename_gb(self):
        filename = generate_vm_filename("ubuntu", "aarch64", "2GB")
        self.assertEqual(filename, "ubuntu.aarch64.2G.qcow2")

    def test_generate_vm_filename_m(self):
        filename = generate_vm_filename("alpine", "i386", "512M")
        self.assertEqual(filename, "alpine.i386.512M.qcow2")

    def test_parse_args_defaults(self):
        args = parse_args([])
        self.assertIsNone(args.iso_path)
        self.assertEqual(args.name, "debian")
        self.assertEqual(args.arch, "x86_64")
        self.assertEqual(args.ram, "1G")
        self.assertEqual(args.disk_size, "12G")

    def test_parse_args_custom(self):
        args = parse_args(
            [
                "/path/to/iso",
                "--name",
                "arch",
                "--arch",
                "aarch64",
                "--ram",
                "4G",
                "--disk-size",
                "20G",
            ]
        )
        self.assertEqual(args.iso_path, "/path/to/iso")
        self.assertEqual(args.name, "arch")
        self.assertEqual(args.arch, "aarch64")
        self.assertEqual(args.ram, "4G")
        self.assertEqual(args.disk_size, "20G")


if __name__ == "__main__":
    unittest.main()
