OBJCC := xcrun clang
OBJCFLAGS := -fobjc-arc -g
LDFLAGS := -fobjc-arc -g

SRCS := $(wildcard *.m)
OBJS := $(SRCS:.m=.o)

main : $(OBJS)
	$(OBJCC) $(LDFLAGS) $^ -o $@

%.o : %.m
	$(OBJCC) $(OBJCFLAGS) -c $< -o $@

clean : 
	rm $(OBJS) main
