<img width="1555" alt="cover" src="https://github.com/user-attachments/assets/6396a00d-605d-4cad-bd0a-03a5d33036d6">

# Interactive Activation and Competition (IAC) Network
This project implements the **Interactive Activation and Competition (IAC)** network, as described by McClelland and Rumelhart. The IAC model demonstrates several key properties of neural networks, including content addressability, robustness to noise, generalization, and plausible default assignments.

------------------------------------------------------------------------

## Overview


<div align="center">
  <img src="https://github.com/user-attachments/assets/98e64065-0026-4774-b933-03205fe2ff6a" alt="SimBrain Image" width="600">
  <p><em>This is derived from SimBrain</em></p>
</div>


The **IAC network** uses competitive pools of interconnected units to model information processing. Each unit represents a feature or hypothesis, and the network processes inputs interactively through positive and negative weight connections. The provided implementation replicates the **Jets and Sharks** example from McClelland's seminal works.

![](/pic/IAC_net_JS.png)

### Key Features:

1.  **Content Addressability**: Retrieve instances or features using partial information.
2.  **Robustness to Noise**: Handle incomplete or incorrect inputs gracefully.
3.  **Generalization**: Identify trends or commonalities across instances.
4.  **Default Assignment**: Assign plausible default values for missing data.

------------------------------------------------------------------------

## How It Works

The network consists of: - **Instance Units**: Representing specific gang members (e.g., Art, Rick, Sam). - **Feature Units**: Representing characteristics (e.g., age, marital status, occupation). - **Positive Connections**: Link between instance units and their features. - **Negative Connections**: Mutual inhibition within competitive pools (e.g., age groups or occupations).

The activation dynamics follow these steps: 1. **Positive Reinforcement**: Units connected by positive weights amplify each other. 2. **Inhibition**: Competitive units inhibit one another through negative weights. 3. **Cycle Processing**: Iteratively updates unit activations to stabilize the network.

------------------------------------------------------------------------

## Code Structure

### Main Components:

1.  **Activation and Weight Management**:
    -   `reset()`: Resets all activations to 0.
    -   `change(name, value)`: Sets the activation of a specific unit.
    -   `changeWeight(name1, name2, value)`: Sets the weight between two units.
2.  **Network Configuration**:
    -   `setWeights()`: Configures the Jets and Sharks network.
    -   `cycle(n)`: Updates unit activations over `n` cycles.
3.  **Visualization**:
    -   `show()`: Displays the current activations for all units.

------------------------------------------------------------------------

## TRY BY YOURSELF! WHY NOT?

### 1. **Content Addressability**

-   Activate units like `Jets` and `Thirties`, then cycle the network to observe the most active instance unit.
-   Demonstrates the network's ability to retrieve Ralph based on partial input.

### 2. **Robustness to Noise**

-   Provide noisy or incomplete inputs (e.g., activate `Forties`, `HighSchool`, `Burglar`, `Divorced`) and observe the network identifying Rick.

### 3. **Generalization**

-   Activate feature units like `Single` to see general trends about single people.

### 4. **Default Assignment**

-   Simulate missing information by removing weights and observe the network's predictions for Ralph's features.

## References

-   McClelland, J. L., & Rumelhart, D. E. (1981). *An interactive activation model of context effects in letter perception: I. An account of basic findings*. Psychological Review, 88(5), 375–407. <https://doi.org/10.1037/0033-295X.88.5.375>
