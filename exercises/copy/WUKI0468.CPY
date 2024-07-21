      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04/06/14
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0468.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/WUKI0468.CPv  $
      *
      *   Rev 2.7   Feb 05 2021 09:43:50   t13ae31
      *AKI01827: G10 SUH VSPKZ DB2
      *
      *   Rev 2.6   Mar 01 2017 10:50:34   t13ae31
      *AKI01493: SUH neue Felder FLEXN OZP
      *
      *   Rev 2.5   Sep 19 2016 07:37:30   t13ae31
      *AKI01460: SUH SMART-HOME OZP KSDS 468 Vertrag
      *
      *   Rev 2.4   Jun 23 2016 08:44:44   t13ae31
      *AKI01438: G10 SUH Angleichung DTA OZP ZT/ZM
      *
      *   Rev 2.3   Mar 30 2016 08:13:16   t13ae31
      *AKI01405: COC  G10 SUH OZP KSDS
      *
      *   Rev 2.2   Oct 08 2014 13:44:44   t13ex78
      *AKI01290: SUH KSDS in DB/2 - Copy-Strecken für Neuprogramme
      *und Änderungen
      *
      *   Rev 2.1   Sep 01 2014 11:27:54   t13ex78
      *AKI01290: SUH KSDS in DB/2 - Standard-OZP für TCKI0468 G10KS
      *DSSUHSA400
      *
      *   Rev 2.0   Jun 4 2014 08:12:25   T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-WUKI0468.
           10 G10VVERTRAGSNR                PIC  X(0015).
           10 G10VMANDANT                   PIC S9(9) COMP.
           10 G10VABSCHLVBNR                PIC S9(9) COMP.
           10 G10VHERKUNFT                  PIC  X(0003).
           10 G10VAENDKZ                    PIC  X(0002).
           10 G10VPIN                       PIC  X(0009).
           10 G10VPIN-PRUEF                 PIC  X(0002).
           10 G10VQUITTNR                   PIC  X(0015).
           10 G10VERSTDATUM                 PIC  X(010).
           10 G10VERSTZEIT                  PIC S9(9) COMP.
           10 G10VANTNR                     PIC  X(0018).
           10 G10V1MERKM                    PIC  X(0020).
           10 G10V1MNR                      PIC  X(0010).
           10 G10V2MERKM                    PIC  X(0020).
           10 G10V2MNR                      PIC  X(0010).
           10 G10VCPBRDNR                   PIC  X(0050).
           10 G10VSPARTE                    PIC  X(0009).
           10 G10VVART                      PIC S9(9) COMP.
           10 G10VBEGINN                    PIC  X(010).
           10 G10VABLAUF                    PIC  X(010).
           10 G10VSTATUS                    PIC  X(0009).
           10 G10VVERSRISIKO                PIC  X(0023).
           10 G10VAENDDAT                   PIC  X(010).
           10 G10VAENDGRD                   PIC  X(0050).
           10 G10VAFBPROZ                   PIC S9(2)V9(3) COMP-3.
           10 G10VBUENDKZ                   PIC  X(0001).
           10 G10VINFO                      PIC  X(0080).
           10 G10VSUMME                     PIC S9(10)V COMP-3.
           10 G10VURWAEHR                   PIC  X(0009).
           10 G10VINKFORM                   PIC  X(0030).
           10 G10VZAHLWEISE                 PIC  X(0009).
           10 G10VNINKBTR                   PIC S9(10)V9(2) COMP-3.
           10 G10VINKJGBTR                  PIC S9(10)V9(2) COMP-3.
           10 G10VFLGDAT                    PIC  X(010).
           10 G10VINKJGNBTR                 PIC S9(8)V9(2) COMP-3.
           10 G10VINKMHNSTF                 PIC  X(0025).
           10 G10VINKMHNDAT                 PIC  X(010).
           10 G10VINKRBTR                   PIC S9(7)V9(2) COMP-3.
           10 G10VINKLFZRAB                 PIC S9(4) COMP.
           10 G10VINKRATZU                  PIC S9(4) COMP.
           10 G10VZEICHNUNG                 PIC  X(0025).
           10 G10VINKFBBTR                  PIC S9(8)V9(2) COMP-3.
           10 G10VINKFBSUMME                PIC S9(9) COMP.
           10 G10VINKELBTRB                 PIC S9(8)V9(2) COMP-3.
           10 G10VINKELBTRN                 PIC S9(8)V9(2) COMP-3.
           10 G10VINKLSTSYN                 PIC  X(010).
           10 G10VINKLSTAGT                 PIC  X(010).
           10 G10VINKLSTBEWG                PIC  X(0061).
           10 G10VINKKDBON                  PIC  X(0026).
           10 G10VINKOSTRAB                 PIC S9(4) COMP.
           10 G10VINKBSA                    PIC  X(0006).
           10 G10VGBSALTXT                  PIC  X(0050).
           10 G10VCGESELL                   PIC  X(0050).
           10 G10VNVDNR                     PIC S9(9) COMP.
           10 G10VVDNAME                    PIC  X(0020).
           10 G10VCVORNR                    PIC  X(0017).
           10 ZUGANG                        PIC  X(0032).
           10 VON                           PIC  X(0032).
           10 BIS                           PIC  X(0032).
           10 G10VAKTIONERG                 PIC  X(0003).
           10 NIL-G10VAKTIONERG             PIC  X(01).
           10 G10VBESTMANAGKZ               PIC  X(0001).
           10 NIL-G10VBESTMANAGKZ           PIC  X(01).
           10 G10VBESCHRBMKZ                PIC  X(0040).
           10 NIL-G10VBESCHRBMKZ            PIC  X(01).
           10 G10VCOCDATUM                  PIC  X(010).
           10 NIL-G10VCOCDATUM              PIC  X(01).
           10 G10VCOCKZ                     PIC  X(0001).
           10 NIL-G10VCOCKZ                 PIC  X(01).
           10 G10VPRKOMBIBEZ                PIC  X(0050).
           10 NIL-G10VPRKOMBIBEZ            PIC  X(01).
           10 G10VKUNDENIDSMH               PIC  X(0010).
           10 NIL-G10VKUNDENIDSMH           PIC  X(01).
           10 G10VFLEXNVSPNL                PIC S9(9)V9(2) COMP-3.
           10 NIL-G10VFLEXNVSPNL            PIC  X(01).
           10 G10VFLEXNVSPKZ                PIC  X(0001).
           10 NIL-G10VFLEXNVSPKZ            PIC  X(01).
           10 G10VFLEXNVGVNL                PIC S9(9)V9(2) COMP-3.
           10 NIL-G10VFLEXNVGVNL            PIC  X(01).
           10 G10VFLEXNVGVKZ                PIC  X(0001).
           10 NIL-G10VFLEXNVGVKZ            PIC  X(01).
           10 DCOP                          PIC  X(0001).
           10 NIL-DCOP                      PIC  X(01).
           10 G10VVSPKZ                     PIC  X(0003).
           10 NIL-G10VVSPKZ                 PIC  X(01).

