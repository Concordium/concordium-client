pipeline {
    agent { label 'windows' }

    stages {
        stage('build') {
            environment {
                GHC_VERSION = '9.6.6'
                BASE_OUTFILE = 's3://distribution.concordium.software/tools/windows/concordium-client'
            }
            steps {
                sh '''\
                    
                    # Extract version number from package.yaml, if not set as parameter
                    [ -z "$VERSION" ] && VERSION=$(awk '/version: / { print $2; exit }' package.yaml)
                    OUTFILE="${BASE_OUTFILE}_${VERSION}.zip"

                    # Fail if file already exists
                    totalFoundObjects=$(aws s3 ls ${OUTFILE} --summarize | grep "Total Objects: " | sed 's/[^0-9]*//g')
                    if [ "$totalFoundObjects" -ne "0" ]; then
                        echo "${OUTFILE} already exists"
                        false
                    fi    

                    # Ensure correct rust env
                    rustup default 1.82-x86_64-pc-windows-gnu

                    # Build project
                    stack build --force-dirty
                    
                    # Zip the binaries
                    mkdir out
                    binDir=$(stack path --local-install-root)/bin
                    (cd $binDir && powershell -Command "Compress-Archive -Path concordium-client.exe,concordium_base.dll,sha_2.dll -DestinationPath concordium-client.zip")
                    mv -f $binDir/concordium-client.zip out/concordium-client.zip

                    # Push to s3
                    aws s3 cp out/concordium-client.zip ${OUTFILE} --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers
                '''.stripIndent()
            }
        }
    }
}
