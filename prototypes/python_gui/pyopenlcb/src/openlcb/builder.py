import sys
try:  
    import pygtk  
    pygtk.require("2.0")  
except:  
    pass  
try:  
    import gtk  
except:  
    print("GTK Not Availible")
    sys.exit(1)

class adder:

    result = 0

    def __init__( self, number1, number2 ):
        self.result = int( number1 ) + int( number2 )

    def giveResult( self ):
        return str(self.result)
        
class adderGui:

    wTree = gtk.Builder()

    def __init__( self ):
        self.builder = gtk.Builder()
        #self.builder.add_from_file("Adder.glade")
        self.window = self.builder.get_object ("windowMain")
        if self.window:
            self.window.connect("destroy", gtk.main_quit)
        self.entry1 = self.builder.get_object ("entry1")
        self.entry2 = self.builder.get_object ("entry2")
        
        dic = { 
            "on_buttonQuit_clicked" : self.quit,
            "on_buttonAdd_clicked" : self.add,
            "on_windowMain_destroy" : self.quit,
        }
        
        self.builder.connect_signals( dic )

    def add(self, widget):
        entry1 = self.builder.get_object ("entry1")
        entry2 = self.builder.get_object ("entry2")
        try:
            thistime = adder( entry1.get_text(), entry2.get_text() )
        except ValueError:
            self.builder.get_object("hboxWarning").show()
            self.builder.get_object("image1").show()
            self.builder.get_object("entryResult").set_text("ERROR")
            return 0
        self.builder.get_object("hboxWarning").hide()
        self.builder.get_object("image1").hide()
        self.builder.get_object("entryResult").set_text(thistime.giveResult())
  
    def quit(self, widget):
        sys.exit(0)
        
#adderGui = adderGui()
#adderGui.window.show()
gtk.main()