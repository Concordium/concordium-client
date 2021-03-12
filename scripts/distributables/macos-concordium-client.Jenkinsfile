pipeline {
    agent { label 'jenkins-worker' }
    environment {
        GHC_VERSION = '8.10.4'
        VERSION = sh(
            returnStdout: true, 
            script: '''\
                # Extract version number if not set as parameter
                [ -z "$VERSION" ] && VERSION=$(awk '/version: / { print $2; exit }' package.yaml)
                echo -n "$VERSION"
            '''.stripIndent()
        )
        OUTFILE = "s3://client-distribution.concordium.com/macos/concordium-client_${VERSION}"
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
                    ghcup rm ghc 8.10.4 || true
                    ghcup set ghc ${GHC_VERSION}-simple

                    # Build project
                    stack build --flag "scientific:integer-simple" --flag "cryptonite:-integer-gmp" --flag "integer-logarithms:-integer-gmp" --flag "hashable:-integer-gmp"
                    
                    mkdir out

                    # Find executable 
                    cp $(find $PWD/.stack-work/install -type f -name "concordium-client") out/
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
