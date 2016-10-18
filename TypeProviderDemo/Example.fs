// adapted from http://blog.mavnn.co.uk/type-providers-from-the-ground-up/


module Sven.Demo.Example

open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open System.Reflection
open System
open Newtonsoft.Json
open Newtonsoft.Json.Linq


type Id () =
    member val UniqueId = Guid() with get, set
    member val Name = "" with get, set

type Port () =
    member val Id = Id() with get, set
    member val Type = "" with get, set

type Node () =
    member val Id = Id() with get, set
    member val Ports = Collections.Generic.List<Port>() with get, set


type InputPort = | InputPort of Port
type OutputPort = | OutputPort of Port

type nodeInstance =
    {
        Node: Node
        InstanceId: Id
        Config: string
    }

module NodeInstance =
    let create node name guid config =
        { Node = node; InstanceId = Id(Name = name, UniqueId = guid); Config = config}

let private nodes = JsonConvert.DeserializeObject<seq<Node>>(IO.File.ReadAllText(@"c:\Temp\Graph.json"))
                    |> Seq.map (fun n -> n.Id.UniqueId.ToString(), n)
                    |> Map.ofSeq

let GetNode id =
    nodes.[id]

let private ports =
    nodes
    |> Map.toSeq
    |> Seq.map (fun (_, node) -> node.Ports)
    |> Seq.concat
    |> Seq.map (fun p -> p.Id.UniqueId.ToString(), p)
    |> Map.ofSeq

let GetPort id =
    ports.[id]

[<TypeProvider>]
type SvenProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()

    let ns = "Sven.Demo.Example.Provided"
    let asm = Assembly.GetExecutingAssembly()

    let addInputPort (inputs: ProvidedTypeDefinition) (port: Port) =
        let port = ProvidedProperty(
                    port.Id.Name, 
                    typeof<InputPort>, 
                    GetterCode = fun args -> 
                        let id = port.Id.UniqueId.ToString()
                        <@@ GetPort id |> InputPort @@>)
        inputs.AddMember(port)

    let addOutputPort (outputs: ProvidedTypeDefinition) (port: Port) =
        let port = ProvidedProperty(
                    port.Id.Name,
                    typeof<OutputPort>,
                    GetterCode = fun args ->
                        let id = port.Id.UniqueId.ToString()
                        <@@ GetPort id |> OutputPort @@>)
        outputs.AddMember(port)

    let addPorts inputs outputs (portList: seq<Port>) =
        portList
        |> Seq.iter (fun port ->
                        match port.Type with
                        | "input" -> addInputPort inputs port
                        | "output" -> addOutputPort outputs port
                        | _ -> failwithf "Unknown port type for port %s/%s" port.Id.Name (port.Id.UniqueId.ToString()))

    let createNodeType id (node: Node) =
        let nodeType = ProvidedTypeDefinition(asm, ns, node.Id.Name, Some typeof<nodeInstance>)
        let ctor = ProvidedConstructor(
                    [
                            ProvidedParameter("Name", typeof<string>)
                            ProvidedParameter("UniqueId", typeof<Guid>)
                            ProvidedParameter("Config", typeof<string>)
                    ],
                    InvokeCode = fun [name;unique;config] -> <@@ NodeInstance.create (GetNode id) (%%name:string) (%%unique:Guid) (%%config:string) @@>)
        nodeType.AddMember(ctor)

        let outputs = ProvidedTypeDefinition("Outputs", Some typeof<obj>)
        let outputCtor = ProvidedConstructor([], InvokeCode = fun args -> <@@ obj() @@>)
        outputs.AddMember(outputCtor)
        outputs.HideObjectMethods <- true

        let inputs = ProvidedTypeDefinition("Inputs", Some typeof<obj>)
        let inputCtor = ProvidedConstructor([], InvokeCode = fun args -> <@@ obj() @@>)
        inputs.AddMember(inputCtor)
        inputs.HideObjectMethods <- true
        addPorts inputs outputs node.Ports

        nodeType.AddMembers([inputs;outputs])

        let outputPorts = ProvidedProperty("OutputPorts", outputs, [], GetterCode = fun args -> <@@ obj() @@>)
        let inputPorts = ProvidedProperty("InputPorts", inputs, [], GetterCode = fun args -> <@@ obj() @@>)

        nodeType.AddMembers([inputPorts;outputPorts])

        nodeType

    let createTypes () = 
        nodes |> Map.map createNodeType |> Map.toList |> List.map (fun (k, v) -> v)
//        let myType = ProvidedTypeDefinition(asm, ns, "MyType", Some typeof<obj>)
//        let myProp = ProvidedProperty("MyProperty", typeof<string>, IsStatic = true, GetterCode = (fun args -> <@@ "Hello world" @@>))
//        
//        myType.AddMember(myProp)
//
//        let ctor = ProvidedConstructor([], InvokeCode = fun args -> <@@ "My internal state" :> obj @@>)
//        myType.AddMember(ctor)
//
//        let ctor2 = ProvidedConstructor(
//            [ProvidedParameter("InnerState", typeof<string>)], InvokeCode = fun args -> <@@ (%%(args.[0]):string) :> obj @@>)
//        myType.AddMember(ctor2)
//
//        let innerState = ProvidedProperty("InnerState", typeof<string>, GetterCode = fun args -> <@@ (%%(args.[0]) :> obj) :?> string @@>)
//        myType.AddMember(innerState)
//
//        [myType]

    do
        this.AddNamespace(ns, createTypes())

[<assembly:TypeProviderAssembly>]
do ()