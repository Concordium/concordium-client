# Key generation for bakers

`concordium-client` supports generating baker credentials that are suitable for starting the node.
The client does not need access to a node to accomplish this.

To generate fresh baker credentials use the following command

```console
$ concordium-client baker generate-keys baker-credentials.json --baker-id 17
```

where
- `baker-credentials.json` is the file where the credentials will be written. This is an optional argument, if it is not provided the output will be printed to standard output.
- `--baker-id` is an id of the baker on the chain. If provided this id will be included in the generated file. This id is needed to start the baker, so if not provided it must be included in `baker-credentials.json` manually later.

If the output file is provided an additional file will be created which contains only the public parts of the baker credentials.
This file will be named the same as the supplied file but with the extension `.pub$EXT` where `$EXT` is the extension of the supplied file.

The client asks for a password to encrypt the generated credentials under.
If an empty password is supplied the generated credentials are emitted in plain.

Example invocation
```console
$ concordium-client baker generate-keys baker-credentials.json --baker-id 17
Enter password for encryption of baker keys (leave blank for no encryption):
Re-enter password for encryption of baker keys:
Keys written to file 'baker-credentials.json'. DO NOT LOSE THIS FILE.
To add a baker to the chain using these keys, use 'concordium-client baker add baker-credentials.json'.
Public keys written to file 'baker-credentials.pub.json'.
```

which resulted in files `baker-credentials.json` and `baker-credentials.pub.json`.
