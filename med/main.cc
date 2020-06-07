// Rendering shit
#include <GL/glew.h>
#include <GL/gl.h>
#include <GL/glext.h>

#include <GLFW/glfw3.h>

// Imgui shit
#include "imgui/imgui.h"
#include "imgui/imgui_impl_opengl3.h"
#include "imgui/imgui_impl_glfw.h"

// Meter shit
#include "Meter/Source.hh"

// med shit
#include "med/Style.hh"
#include "med/SourceRenderer.hh"

// stl
#include <map>

int main() {
  Style style;

  assert(glfwInit());

  IMGUI_CHECKVERSION();
  ImGui::CreateContext();

  GLFWwindow* window = glfwCreateWindow(640, 480, "med", NULL, NULL);
  assert(window);

  glfwMakeContextCurrent(window);
  glfwSwapInterval(1);

  assert(glewInit() == GLEW_OK);

  ImGui_ImplGlfw_InitForOpenGL(window, true);
  ImGui_ImplOpenGL3_Init();

  style.init();

  std::map<std::string, Meter::Source> sources;
  sources["idk"];

  ImGuiIO &io = ImGui::GetIO();
  io.WantCaptureKeyboard = true;
  io.ConfigFlags |= ImGuiConfigFlags_DockingEnable;
  io.ConfigDockingWithShift = false;

  bool exited = false;

  while(!glfwWindowShouldClose(window) && !exited) {
    glfwPollEvents();
    glClearColor(0.1f, 0.1f, 0.1f, 1.00f);
    glClear(GL_COLOR_BUFFER_BIT);

    ImGui_ImplOpenGL3_NewFrame();
    ImGui_ImplGlfw_NewFrame();
    ImGui::NewFrame();

    auto bg = style.background_color;
    ImGui::PushStyleColor(ImGuiCol_WindowBg, ImVec4(bg.col[0], bg.col[1], bg.col[2], 1.0f));

    ImGui::DockSpaceOverViewport();

    for(auto &[name, source]: sources) {
      render_source(name, source, style);
    }

    style.window();

    ImGui::ShowDemoWindow();

    ImGui::PopStyleColor();

    ImGui::Render();
    ImGui_ImplOpenGL3_RenderDrawData(ImGui::GetDrawData());

    int display_w, display_h;
    glfwGetFramebufferSize(window, &display_w, &display_h);
    glViewport(0, 0, display_w, display_h);
    glfwSwapBuffers(window);
  }

  ImGui_ImplOpenGL3_Shutdown();
  ImGui_ImplGlfw_Shutdown();
  ImGui::DestroyContext();
}
