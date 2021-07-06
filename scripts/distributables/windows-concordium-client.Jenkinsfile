pipeline {
    agent { label 'windows' }

    stages {
        stage('build') {
            environment {
                GHC_VERSION = '8.10.4'
                BASE_OUTFILE = 's3://distribution.concordium.software/tools/windows/concordium-client'
            }
            steps {
                sh '''\
                    
                    # Extract version number from package.yaml, if not set as parameter
                    [ -z "$VERSION" ] && VERSION=$(awk '/version: / { print $2; exit }' package.yaml)
                    OUTFILE="${BASE_OUTFILE}_${VERSION}.exe"

                    # Fail if file already exists
                    totalFoundObjects=$(aws s3 ls ${OUTFILE} --summarize | grep "Total Objects: " | sed 's/[^0-9]*//g')
                    if [ "$totalFoundObjects" -ne "0" ]; then
                        echo "${OUTFILE} already exists"
                        false
                    fi    

                    # Ensure correct rust env
                    rustup default 1.53-x86_64-pc-windows-gnu

                    # Configure project to use custom GHC
                    # cygpath -w converts msys2 path to windows path
                    echo "setup-info: 
                      ghc:  
                        windows64-integersimple: 
                          ${GHC_VERSION}:  
                            url: https://s3-eu-west-1.amazonaws.com/static-libraries.concordium.com/ghc-8.10.4-x86_64-unknown-mingw32.tar.xz
                            sha256: 6aa0ea598452e7b439d8ad1cd0cf3d7267317df4b266daf0359b1faabba6a588
                            
                    ghc-variant: integersimple" >> stack.yaml

                    # Build project
                    stack build --flag "scientific:integer-simple" --flag "cryptonite:-integer-gmp" --flag "integer-logarithms:-integer-gmp" --flag "hashable:-integer-gmp" --force-dirty
                    
                    mkdir out

                    # Find executable 
                    cp $(find $PWD/.stack-work/install -type f -name "concordium-client.exe") out/

                    # Push to s3
                    aws s3 cp out/concordium-client.exe ${OUTFILE} --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers
                '''.stripIndent()
            }
        }
    }
}
