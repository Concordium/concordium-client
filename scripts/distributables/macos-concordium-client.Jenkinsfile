pipeline {
    agent { label 'jenkins-worker' }
    environment {
        GHC_VERSION = '9.0.2'
        RUST_VERSION = '1.53'
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
                    # Install correct version of rust.
                    rustup default $RUST_VERSION

                    # due to a bug in stack and flag propagation we have to do this hack
                    sed -i '' "s/default: False/default: True/g" deps/concordium-base/package.yaml
                    sed -i '' "s/default: False/default: True/g" deps/concordium-base/concordium-base.cabal

                    # At present concordium-client does not build in a static build with any version of ghc more recent than 8.8.4.
                    stack build --compiler=ghc-9.0.2 --flag concordium-client:-middleware

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
                    aws s3 cp out/concordium-client ${OUTFILE} --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers
                '''.stripIndent()
            }
        }
    }
}
