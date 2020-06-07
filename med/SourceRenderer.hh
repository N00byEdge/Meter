#pragma once

#include "Meter/Source.hh"

#include <string>

struct Style;

void render_source(std::string const &name, Meter::Source &source, Style &style);
