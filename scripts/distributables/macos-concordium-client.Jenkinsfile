pipeline {
    agent { label 'jenkins-worker' }
    environment {
        GHC_VERSION = '8.8.4'
        VERSION = sh(
            returnStdout: true, 
            script: '''\
                # Extract version number if not set as parameter
                [ -z "$VERSION" ] && VERSION=$(awk '/version: / { print $2; exit }' package.yaml)
                echo -n "$VERSION"
            '''.stripIndent()
        )
        OUTFILE = "s3://distribution.concordium.software/tools/macos/concordium-client_${VERSION}"
    }
    stages {
        stage('precheck') {
            steps {
                sh '''\
                    # Fail if file already exists
                    totalFoundObjects=$(aws s3 ls ${OUTFILE} --summarize | grep "Total Objects: " | sed 's/[^0-9]*//g')
                    if [ "$totalFoundObjects" -ne "0" ]; then
                        echo "${OUTFILE} already exists"
                        false
                    fi
                '''.stripIndent()
            }
        }
        stage('build') {
            agent { label 'mac' }
            steps {
                sh '''\
                    # Ensure using custom version of ghc
                    # Stack is refusing for some reason to use the ghc installed by ghcup :/ so I'm installing it via stack.
                    rm -rf ~/.stack/*
                    cat <<'EOF' > ~/.stack/config.yaml
                    setup-info:
                      ghc:
                         macosx-custom-integer-simple:
                             8.8.4:
                                  url: "https://s3-eu-west-1.amazonaws.com/static-libraries.concordium.com/ghc-8.8.4-x86_64-apple-darwin.tar.xz"

                    ghc-variant: integer-simple
                    EOF
                    
                    sed -i '' "s/default: False/default: True/g" deps/concordium-base/package.yaml

                    # Build project
                    # Note that we have to copy the haddock binary into the specified path or else stack will fail.
                    # As we don't spawn a fresh instance each time, I did build `cabal install haddock --install-method=copy` and then we can copy the binary into the required path. If it goes missing, just build it manually again
                    (stack build --compiler=ghc-8.8.4 --flag "scientific:integer-simple" --flag "cryptonite:-integer-gmp" --flag "integer-logarithms:-integer-gmp" --flag "hashable:-integer-gmp" || ( cp ~/.cabal/bin/haddock /Users/administrator/.stack/programs/x86_64-osx/ghc-custom-integer-simple-8.8.4/bin/haddock-8.8.4 && stack build --compiler=ghc-8.8.4 --flag "scientific:integer-simple" --flag "cryptonite:-integer-gmp" --flag "integer-logarithms:-integer-gmp" --flag "hashable:-integer-gmp"))
                           
                    mkdir out

                    # Find executable
                    cp $(find .stack-work/install -type f -name concordium-client) out/concordium-client
                '''.stripIndent()
                stash includes: 'out/concordium-client', name: 'release'
            }
        }
        stage('Publish') {
            steps {
                unstash 'release'
                sh '''\
                    aws s3 cp out/concordium-client ${OUTFILE}
                '''.stripIndent()
            }
        }
    }
}
