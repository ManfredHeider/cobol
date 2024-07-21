      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07/03/18
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0492.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WUKI0492.CPv  $
      *
      *   Rev 2.2   Apr 15 2020 08:15:06   t13ex78
      *AKI01771: Standard OZP ZTKI0492
      *
      *   Rev 2.1   May 24 2018 08:02:18   t13ae31
      *AKI01575: G70 Umstellung KSDS auf DB2 OZP Vertrag
      *
      *   Rev 2.0   Mar 7 2018 13:13:32   T13AE31
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-WUKI0492.
           10 G70VVERTRAGSNR                PIC  X(0015).
           10 G70VLFDNR                     PIC  X(0001).
           10 G70VMANDANT                   PIC S9(9) COMP.
           10 G70VAENDKZ                    PIC  X(0002).
           10 G70VPRODSCHL                  PIC  X(0002).
           10 G70VLAUFZEIT                  PIC  X(0002).
           10 G70VBEGINN                    PIC  X(0008).
           10 G70VABLAUF                    PIC  X(0008).
           10 G70VSUMME                     PIC S9(8)V9(2) COMP-3.
           10 G70VBEITRAG                   PIC S9(5)V9(2) COMP-3.
           10 G70VKTOSTAKZ                  PIC  X(0001).
           10 G70VBEWERTZAHL                PIC S9(3)V9(5) COMP-3.
           10 G70VTARIFKZ                   PIC  X(0001).
           10 G70VZINSALLG                  PIC S9(3)V9(2) COMP-3.
           10 G70VDARLZINS                  PIC S9(3)V9(2) COMP-3.
           10 G70VDISPROZ                   PIC S9(3)V9(2) COMP-3.
           10 G70VFMINANSPAR                PIC S9(8)V9(2) COMP-3.
           10 G70VENDSTEULZ                 PIC  X(0008).
           10 G70VDATWOBPRAE                PIC  X(0008).
           10 G70VDATZUTAUSSI               PIC  X(0008).
           10 G70VDATMINANSP                PIC  X(0008).
           10 G70VPARTINFO                  PIC  X(0020).
           10 G70VABSCHLVBNR                PIC S9(9) COMP.
           10 G70VMINANSPPRO                PIC S9(3)V9(2) COMP-3.
           10 G70VZAHLW                     PIC  X(0002).
           10 G70VSPARBETRAG                PIC S9(9)V9(2) COMP-3.
           10 G70VBONUSVORJ                 PIC S9(9)V9(2) COMP-3.
           10 G70VBWZVJ                     PIC S9(3)V9(5) COMP-3.
           10 G70VZINSLFDJ                  PIC S9(9)V9(2) COMP-3.
           10 G70VWKZ                       PIC  X(0003).
           10 G70VLETZTAEND                 PIC  X(0008).
           10 G70VSCHLWOHNPRG               PIC  X(0002).
           10 G70VBASERHOEH                 PIC S9(3)V9(2) COMP-3.
           10 G70VEHELKZ                    PIC  X(0001).
           10 G70VBLANKODARL                PIC  X(0001).
           10 G70VERMITTKZ                  PIC  X(0001).
           10 G70VDATSPERFR                 PIC  X(0008).
           10 G70VZINSVOR                   PIC S9(9)V9(2) COMP-3.
           10 G70VDATAGEIN                  PIC  X(0008).
           10 G70VFINPROD                   PIC  X(0002).
           10 G70VDZINSFAB                  PIC  X(0008).
           10 G70VDZINSFBIS                 PIC  X(0008).
           10 G70VDARLZINOM                 PIC S9(3)V9(3) COMP-3.
           10 G70VDARLZIEFF                 PIC S9(3)V9(3) COMP-3.
           10 G70VANNURATE                  PIC S9(9)V9(2) COMP-3.
           10 G70VLZFIN                     PIC  X(0003).
           10 G70VSONDTILJ                  PIC S9(9)V9(2) COMP-3.
           10 G70VDARLHOEHE                 PIC S9(9)V9(2) COMP-3.
           10 G70VDARLSALDO                 PIC S9(9)V9(2) COMP-3.
           10 G70VDATAFBEG                  PIC  X(0008).
           10 G70VBONUSFAKT                 PIC S9(2)V9(2) COMP-3.
           10 G70VBONUSAKT                  PIC S9(9)V9(2) COMP-3.
           10 G70VZPGEN                     PIC  X(0002).
           10 G70VMTLEINZBETR               PIC S9(7)V9(2) COMP-3.
           10 G70VMTLEINZDAT                PIC  X(0008).
           10 G70VJAERLEINZBETR             PIC S9(7)V9(2) COMP-3.
           10 G70VJAERLEINZDAT              PIC  X(0008).
           10 G70VDYNJAEREINZ               PIC S9(3)V9(2) COMP-3.
           10 G70VMINBEWZAHL                PIC S9(4)V9(2) COMP-3.
           10 G70VINKASSO                   PIC  X(0001).
           10 G70VSTEULFZDAT                PIC  X(0008).
           10 G70VSUMVLEINZ                 PIC S9(7)V9(2) COMP-3.
           10 G70VWOPKZ                     PIC  X(0001).
           10 G70VWOPVJ                     PIC  X(0004).
           10 G70VWOPVVJ                    PIC  X(0004).
           10 G70VDAUZULKZ                  PIC  X(0001).
           10 G70VFOERDRELEV                PIC S9(7)V9(2) COMP-3.
           10 G70VSALDOWFKTO                PIC S9(7)V9(2) COMP-3.
           10 G70VSUMAVWL                   PIC S9(7)V9(2) COMP-3.
           10 G70VSUMFOERVJ                 PIC S9(7)V9(2) COMP-3.
           10 G70VVARIWECHS                 PIC  X(0001).
           10 G70VFINANZKZ                  PIC  X(0001).
           10 G70VZUTEILANW                 PIC  X(0008).
           10 G70VDARLAKTEKZ                PIC  X(0001).
           10 G70VENDBINDFRI                PIC  X(0010).
           10 G70VDLETZTAKT                 PIC  X(0008).
           10 G70VZIELTILG                  PIC S9(3)V9(2) COMP-3.
           10 G70VMEHRZUTEIL                PIC S9(7)V9(2) COMP-3.
           10 G70VABTRETBLZ                 PIC  X(0008).
           10 G70VTARIFUMST                 PIC S9(5)V9(2) COMP-3.
           10 G70VANFTILG                   PIC S9(3)V9(2) COMP-3.
           10 G70VLETZTZUTEIL               PIC  X(0008).
           10 G70VANWZUTEIVO                PIC  X(0008).
           10 G70VANSPWOP                   PIC S9(7)V9(2) COMP-3.
           10 G70VSPARZULAB                 PIC  X(0008).
           10 G70VVWLKZ                     PIC  X(0001).
           10 G70VIMMOFITKZ                 PIC  X(0001).
           10 G70VWERBVERBOT                PIC  X(0001).
           10 G70VGUTHABBEGR                PIC  X(0008).
           10 G70VMAXVZINSGH                PIC S9(9)V9(2) COMP-3.
           10 G70VMIETKAUT                  PIC  X(0001).
           10 G70VPASSWORT                  PIC  X(0001).
           10 G70VLETZTKTOAZ                PIC  X(0008).
           10 G70VNRAKTKTOAZ                PIC  X(0007).
           10 G70VNVERANLBE                 PIC  X(0008).
           10 G70VVLPERSONAL                PIC  X(0001).
           10 G70VONLPROD                   PIC  X(0050).
           10 G70VONLART                    PIC  X(0050).
           10 G70VFRONTENDT                 PIC  X(0008).
           10 G70VABBVERS                   PIC  X(0007).
           10 G70VJAHRZULAGE.
             49 G70VJAHRZULAGE-LEN          PIC S9(4) COMP.
             49 G70VJAHRZULAGE-TEXT         PIC X(105).
           10 ZUGANG                        PIC  X(0032).
           10 VON                           PIC  X(0032).
           10 BIS                           PIC  X(0032).
           10 DCOP                          PIC  X(0001).
           10 NIL-DCOP                      PIC  X(01).
           10 G70VBONUSART                  PIC  X(0002).
           10 G70VBONUSBEGDATUM             PIC  X(0008).
           10 G70VTUBAKTION                 PIC  X(0001).
           10 G70VSONDERZAHLUNG             PIC S9(3)V9(2) COMP-3.
           10 G70VSONDERTILGUNG             PIC S9(3)V9(2) COMP-3.

