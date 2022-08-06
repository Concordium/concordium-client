pipeline {
    agent { label 'windows' }

    stages {
        stage('build') {
            environment {
                GHC_VERSION = '9.0.2'
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
                    rustup default 1.62-x86_64-pc-windows-gnu

                    # Build project
                    stack build --force-dirty --flag concordium-client:-middleware
                    
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
