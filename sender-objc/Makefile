OBJCC := xcrun clang
OBJCFLAGS := -fobjc-arc -g -O2 -flto
LDFLAGS := -fobjc-arc -g -O2 -flto

SRCS := $(wildcard *.m)
OBJS := $(SRCS:.m=.o)

main : $(OBJS)
	$(OBJCC) $(LDFLAGS) $^ -o $@

%.o : %.m
	$(OBJCC) $(OBJCFLAGS) -c $< -o $@

clean : 
	rm $(OBJS) main
