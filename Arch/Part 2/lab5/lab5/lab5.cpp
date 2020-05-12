#include "opencv2/opencv.hpp"
#include <iostream>

using namespace std;
using namespace cv;

int main() {

    // Create a VideoCapture object and use camera to capture the video
    VideoCapture cap(0);

    // Check if camera opened successfully
    if (!cap.isOpened()) {
        cout << "Error opening video stream" << endl;
        return -1;
    }

    // Default resolution of the frame is obtained.The default resolution is system dependent. 
    int frame_width = static_cast<int>(cap.get(CAP_PROP_FRAME_WIDTH));
    int frame_height = static_cast<int>(cap.get(CAP_PROP_FRAME_HEIGHT));

    // Define the codec and create VideoWriter object.The output is stored in 'outcpp.avi' file. 
    VideoWriter video("..\\result.avi", cv::VideoWriter::fourcc('M', 'J', 'P', 'G'), CAP_PROP_FPS, Size(frame_width, frame_height));
    while (1) {
        Mat frame;
        Mat img = imread("..\\opencv.png");
        Mat result;

        // Capture frame-by-frame 
        cap >> frame;

        // If the frame is empty, break immediately
        if (frame.empty())
            break;

        addWeighted(img, 0.1, frame, 0.9, 0.0, result);

        // Write the frame into the file 'outcpp.avi'
        video.write(result);

        // Display the resulting frame
        imshow("Result", frame);

        // Press  ESC on keyboard to  exit
        char c = static_cast<char>(waitKey(1));
        if (c == 27)
            break;
    }

    // When everything done, release the video capture and write object
    cap.release();
    video.release();

    // Closes all the windows
    destroyAllWindows();

    return 0;
}
