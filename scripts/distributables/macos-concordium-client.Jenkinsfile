pipeline {
    agent { label 'jenkins-worker' }
    environment {
        GHC_VERSION = '9.6.4'
        RUST_VERSION = '1.68'
        VERSION = sh(
            returnStdout: true,
            script: '''\
                # Extract version number if not set as parameter
                [ -z "$VERSION" ] && VERSION=$(awk '/version: / { print $2; exit }' package.yaml)
                echo -n "$VERSION"
            '''.stripIndent()
        )
        PACKAGE = "concordium-client-${VERSION}-unsigned.pkg"
        OUTFILE = "s3://distribution.concordium.software/tools/macos/${PACKAGE}"
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

                    # At present concordium-client does not build in a static build with any version of ghc more recent than 8.8.4.
                    # Thus, we need to create an installer package.
                    scripts/distributables/macOS-package/build.sh --build "$VERSION"

                    mkdir out

                    # Find executable
                    cp scripts/distributables/macOS-package/build/${PACKAGE} out/concordium-client.pkg
                '''.stripIndent()
                stash includes: 'out/concordium-client.pkg', name: 'release'
            }
        }
        stage('Publish') {
            steps {
                unstash 'release'
                sh '''\
                    aws s3 cp out/concordium-client.pkg ${OUTFILE} --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers
                '''.stripIndent()
            }
        }
    }
}
