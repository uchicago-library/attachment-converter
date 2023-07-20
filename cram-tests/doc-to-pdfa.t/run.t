Testing doc to pdfa conversion:
  $ ../../conversion-scripts/soffice-wrapper.sh -i doc -o pdf < test-doc.doc > converted-doc-to-pdfa.pdf



Output:
  $ file --mime-type converted-doc-to-pdfa.pdf
  converted-doc-to-pdfa.pdf: application/pdf
  $ pdftotext -eol unix converted-doc-to-pdfa.pdf
  $ cat converted-doc-to-pdfa.txt
  Lorem ipsum
  Lorem ipsum dolor sit amet, consectetur adipiscing
  elit. Nunc ac faucibus odio.
  Vestibulum neque massa, scelerisque sit amet ligula eu, congue molestie mi. Praesent ut
  varius sem. Nullam at porttitor arcu, nec lacinia nisi. Ut ac dolor vitae odio interdum
  condimentum. Vivamus dapibus sodales ex, vitae malesuada ipsum cursus
  convallis. Maecenas sed egestas nulla, ac condimentum orci. Mauris diam felis,
  vulputate ac suscipit et, iaculis non est. Curabitur semper arcu ac ligula semper, nec luctus
  nisl blandit. Integer lacinia ante ac libero lobortis imperdiet. Nullam mollis convallis ipsum,
  ac accumsan nunc vehicula vitae. Nulla eget justo in felis tristique fringilla. Morbi sit amet
  tortor quis risus auctor condimentum. Morbi in ullamcorper elit. Nulla iaculis tellus sit amet
  mauris tempus fringilla.
  Maecenas mauris lectus, lobortis et purus mattis, blandit dictum tellus.
  
  
  
  Maecenas non lorem quis tellus placerat varius.
  
  
  
  Nulla facilisi.
  
  
  
  Aenean congue fringilla justo ut aliquam.
  
  
  
  Mauris id ex erat. Nunc vulputate neque vitae justo facilisis, non condimentum ante
  sagittis.
  
  
  
  Morbi viverra semper lorem nec molestie.
  
  
  
  Maecenas tincidunt est efficitur ligula euismod, sit amet ornare est vulputate.
  
  12
  10
  8
  Column 1
  Column 2
  Column 3
  
  6
  4
  2
  0
  Row 1
  
  Row 2
  
  Row 3
  
  Row 4
  
  In non mauris justo. Duis vehicula mi vel mi pretium, a viverra erat efficitur. Cras aliquam
  est ac eros varius, id iaculis dui auctor. Duis pretium neque ligula, et pulvinar mi placerat
  et. Nulla nec nunc sit amet nunc posuere vestibulum. Ut id neque eget tortor mattis
  tristique. Donec ante est, blandit sit amet tristique vel, lacinia pulvinar arcu. Pellentesque
  scelerisque fermentum erat, id posuere justo pulvinar ut. Cras id eros sed enim aliquam
  lobortis. Sed lobortis nisl ut eros efficitur tincidunt. Cras justo mi, porttitor quis mattis vel,
  ultricies ut purus. Ut facilisis et lacus eu cursus.
  In eleifend velit vitae libero sollicitudin euismod. Fusce vitae vestibulum velit. Pellentesque
  vulputate lectus quis pellentesque commodo. Aliquam erat volutpat. Vestibulum in egestas
  velit. Pellentesque fermentum nisl vitae fringilla venenatis. Etiam id mauris vitae orci
  maximus ultricies.
  
  Cras fringilla ipsum magna, in fringilla dui commodo
  a.
  Lorem ipsum
  
  Lorem ipsum Lorem ipsum
  
  1
  
  In eleifend velit vitae libero sollicitudin euismod.
  
  Lorem
  
  2
  
  Cras fringilla ipsum magna, in fringilla dui commodo Ipsum
  a.
  
  3
  
  Aliquam erat volutpat.
  
  Lorem
  
  4
  
  Fusce vitae vestibulum velit.
  
  Lorem
  
  5
  
  Etiam vehicula luctus fermentum.
  
  Ipsum
  
  Etiam vehicula luctus fermentum. In vel metus congue, pulvinar lectus vel, fermentum dui.
  Maecenas ante orci, egestas ut aliquet sit amet, sagittis a magna. Aliquam ante quam,
  pellentesque ut dignissim quis, laoreet eget est. Aliquam erat volutpat. Class aptent taciti
  sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Ut ullamcorper
  justo sapien, in cursus libero viverra eget. Vivamus auctor imperdiet urna, at pulvinar leo
  posuere laoreet. Suspendisse neque nisl, fringilla at iaculis scelerisque, ornare vel dolor. Ut
  et pulvinar nunc. Pellentesque fringilla mollis efficitur. Nullam venenatis commodo
  imperdiet. Morbi velit neque, semper quis lorem quis, efficitur dignissim ipsum. Ut ac lorem
  sed turpis imperdiet eleifend sit amet id sapien.
  
  Lorem ipsum dolor sit amet, consectetur adipiscing
  elit.
  Nunc ac faucibus odio. Vestibulum neque massa, scelerisque sit amet ligula eu, congue molestie mi.
  Praesent ut varius sem. Nullam at porttitor arcu, nec lacinia nisi. Ut ac dolor vitae odio interdum
  condimentum. Vivamus dapibus sodales ex, vitae malesuada ipsum cursus convallis. Maecenas sed
  egestas nulla, ac condimentum orci. Mauris diam felis, vulputate ac suscipit et, iaculis non est.
  Curabitur semper arcu ac ligula semper, nec luctus nisl blandit. Integer lacinia ante ac libero lobortis
  imperdiet. Nullam mollis convallis ipsum, ac accumsan nunc vehicula vitae. Nulla eget justo in felis
  tristique fringilla. Morbi sit amet tortor quis risus auctor condimentum. Morbi in ullamcorper elit. Nulla
  iaculis tellus sit amet mauris tempus fringilla.
  
  Maecenas mauris lectus, lobortis et purus mattis, blandit
  dictum tellus.
  Maecenas non lorem quis tellus placerat varius. Nulla facilisi. Aenean congue fringilla justo ut aliquam.
  Mauris id ex erat. Nunc vulputate neque vitae justo facilisis, non condimentum ante sagittis. Morbi
  viverra semper lorem nec molestie. Maecenas tincidunt est efficitur ligula euismod, sit amet ornare est
  vulputate.
  In non mauris justo. Duis vehicula mi vel mi pretium, a viverra erat efficitur. Cras aliquam est ac eros
  varius, id iaculis dui auctor. Duis pretium neque ligula, et pulvinar mi placerat et. Nulla nec nunc sit
  amet nunc posuere vestibulum. Ut id neque eget tortor mattis tristique. Donec ante est, blandit sit amet
  tristique vel, lacinia pulvinar arcu. Pellentesque scelerisque fermentum erat, id posuere justo pulvinar
  ut. Cras id eros sed enim aliquam lobortis. Sed lobortis nisl ut eros efficitur tincidunt. Cras justo mi,
  porttitor quis mattis vel, ultricies ut purus. Ut facilisis et lacus eu cursus.
  
  In eleifend velit vitae libero sollicitudin euismod.
  Fusce vitae vestibulum velit. Pellentesque vulputate lectus quis pellentesque commodo. Aliquam erat
  volutpat. Vestibulum in egestas velit. Pellentesque fermentum nisl vitae fringilla venenatis. Etiam id
  mauris vitae orci maximus ultricies. Cras fringilla ipsum magna, in fringilla dui commodo a.
  Etiam vehicula luctus fermentum. In vel metus congue, pulvinar lectus vel, fermentum dui. Maecenas
  ante orci, egestas ut aliquet sit amet, sagittis a magna. Aliquam ante quam, pellentesque ut dignissim
  quis, laoreet eget est. Aliquam erat volutpat. Class aptent taciti sociosqu ad litora torquent per conubia
  nostra, per inceptos himenaeos. Ut ullamcorper justo sapien, in cursus libero viverra eget. Vivamus
  auctor imperdiet urna, at pulvinar leo posuere laoreet. Suspendisse neque nisl, fringilla at iaculis
  scelerisque, ornare vel dolor. Ut et pulvinar nunc. Pellentesque fringilla mollis efficitur. Nullam venenatis
  commodo imperdiet. Morbi velit neque, semper quis lorem quis, efficitur dignissim ipsum. Ut ac lorem
  sed turpis imperdiet eleifend sit amet id sapien.
  
  
