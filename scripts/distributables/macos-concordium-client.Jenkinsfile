pipeline {
    agent none
    environment {
        GHC_VERSION = '8.8.4'
        BASE_OUTFILE = 's3://static-libraries.concordium.com/dist-macos/concordium-client'
    }
    stages {
        stage('precheck') {
            agent { label 'jenkins-worker' }
            steps {
                sh '''\
                    # Extract version number from package.yaml, if not set as parameter
                    [ -z "$VERSION" ] && VERSION=$(awk '/version: / { print $2; exit }' package.yaml)
                    OUTFILE="${BASE_OUTFILE}_${VERSION}"

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
                    ghcup rm ghc 8.8.4 || true
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
            agent { label 'jenkins-worker' }
            steps {
                unstash 'release'
                sh '''\
                    # Push to s3
                    # Extract version number from package.yaml, if not set as parameter
                    [ -z "$VERSION" ] && VERSION=$(awk '/version: / { print $2; exit }' package.yaml)
                    OUTFILE="${BASE_OUTFILE}_${VERSION}"
                    aws s3 cp out/concordium-client ${OUTFILE} --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers
                '''.stripIndent()
            }
        }
    }
}
