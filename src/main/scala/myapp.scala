
package dummy

import javafx.stage._

import Builder._

object MyApp
{

	def Init
	{
		ModuleManager.Add(MyActor)
		ModuleManager.Add(ProcessManager)
	}

	def handler(ev:MyEvent)
	{

	}

	def Start(primaryStage:Stage)
	{
		val blob="""
			|<vbox>
			|<tabpane id="{maintabpane}">
			|<tab caption="Main">
			|</tab>
			|<tab caption="Systemlog">
			|<webview id="{systemlog}"/>
			|</tab>
			|<tab caption="Execqueue">
			|<webview id="{execqueue}"/>
			|</tab>
			|<tab caption="Queuelog">
			|<webview id="{execqueuelog}"/>
			|</tab>
			|</tabpane>
			|</vbox>
		""".stripMargin

		MyStage(id="{mainstage}",s=primaryStage,title="dummy",blob=blob,handler=handler)
	}
	
	def Stop
	{

	}	

}

		