pipeline {
    agent any
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
        OUTFILE = "s3://client-distribution.concordium.com/linux/concordium-client_${VERSION}"
    }
    stages {
        stage('ecr-login') {
            steps {
                sh 'aws ecr get-login-password \
                        --region eu-west-1 \
                    | docker login \
                        --username AWS \
                        --password-stdin 192549843005.dkr.ecr.eu-west-1.amazonaws.com'
            }
        }
        stage('precheck') {
            steps {
                sh '''\
                    # Fail if file already exists
                    totalFoundObjects=$(aws s3 ls ${OUTFILE} --summarize | grep "Total Objects: " | sed "s/[^0-9]*//g")
                    if [ "$totalFoundObjects" -ne "0" ]; then
                        echo "${OUTFILE} already exists"
                        false
                    fi
                '''.stripIndent()
            }
        }
        stage('build') {
            steps {
                sh './scripts/distributables/linux-build-and-push-distributable-concordium-client.sh'
            }
        }
    }
}
