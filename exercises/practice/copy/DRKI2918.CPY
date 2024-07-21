      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.06.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2918.CPY  $
      *    $Revision:   2.9  $
      *
      *    $Log:   L:/mf/CPY/DRKI2918.CPv  $
      *
      *   Rev 2.9   Mar 01 2017 10:50:16   t13ae31
      *AKI01493: SUH neue Felder FLEXN OZP
      *
      *   Rev 2.8   Sep 19 2016 07:37:02   t13ae31
      *AKI01460: SUH SMART-HOME OZP KSDS 468 Vertrag
      *
      *   Rev 2.7   Jun 23 2016 09:09:16   t13ae31
      *AKI01438: G10 SUH Angleichung DTA OZP ZX
      *
      *   Rev 2.6   Dec 11 2014 10:20:34   t13ex78
      *AKI01290: SUH KSDS in DB/2 - ZXKI0468
      *
      *   Rev 2.5   Dec 10 2014 13:32:22   t13ex78
      *AKI01290: SUH KSDS in DB/2 - ZXKI0468
      *
      *   Rev 2.4   Dec 09 2014 10:03:50   t13ex78
      *AKI01290: SUH KSDS in DB/2 - ZXKI0468
      *
      *   Rev 2.3   Dec 08 2014 14:46:40   t13ex78
      *AKI01290: SUH KSDS in DB/2 - ZXKI0468
      *
      *   Rev 2.2   Dec 08 2014 14:20:40   t13ex78
      *AKI01290: SUH KSDS in DB/2 - ZXKI0468
      *
      *   Rev 2.1   Sep 01 2014 11:26:38   t13ex78
      *AKI01290: SUH KSDS in DB/2 - List-OZP für TCKI0468 G10KSDSSU
      *HSA400
      *
      *   Rev 2.0   Jun 05 2014 08:56:24   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI2918                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXKI0468                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: T.Flierl                                         *
      *    DATUM:    04.06.2014                                       *
      *****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01290!04.06.14!1.0 ! NEU                       ! T.Flierl *
      * -------------------------------------------------------------*
      * AKI01438!12.05.16!2.7 ! Erweiterung neue Felder   ! P.Hübner *
      * -------------------------------------------------------------*
050816* AKI01460!05.08.16!2.8 ! Erweiterung neue Felder   ! P.Hübner *
      *         !        !    !                           !          *
010217* AKI01493!01.02.17!2.9 ! Erweiterung neue Felder   ! P.Hübner *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0963                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
 
       01 PI-DRKI2918.
          05 PI-DRKI2918-PROGRAMM                PIC  X(008).
          05 PI-DRKI2918-UFKT-CODE               PIC  9(002).
          05 PI-DRKI2918-DATEN.
             10 PI-DRKI2918-G10VVERTRAGSNR       PIC  X(015).
             10 PI-DRKI2918-G10VMANDANT          PIC S9(9) COMP.
 
       01 PO-DRKI2918.
 
          05 PO-DRKI2918-RC                      PIC  X(003).
             88 PO-DRKI2918-RC-OK                VALUE 'OK '.
             88 PO-DRKI2918-RC-NOT-FOUND         VALUE 'NFD'.
             88 PO-DRKI2918-RC-EOF               VALUE 'EOF'.
 
          05 PO-DRKI2918-DATEN.
           10 PO-DRKI2918-G10VVERTRAGSNR         PIC X(15).
           10 PO-DRKI2918-REST.
             15 PO-DRKI2918-G10VABLAUF           PIC  X(010).
             15 PO-DRKI2918-G10VABSCHLVBNR       PIC S9(9) COMP.
             15 PO-DRKI2918-G10VAENDDAT          PIC  X(010).
             15 PO-DRKI2918-G10VAENDGRD          PIC  X(0050).
             15 PO-DRKI2918-G10VAENDKZ           PIC  X(0002).
             15 PO-DRKI2918-G10VAFBPROZ          PIC S9(2)V9(3) COMP-3.
120516       15 PO-DRKI2918-G10VAKTIONERG        PIC  X(3).
             15 PO-DRKI2918-G10VANTNR            PIC  X(0018).
             15 PO-DRKI2918-G10VBEGINN           PIC  X(010).
120516       15 PO-DRKI2918-G10VBESCHRBMKZ       PIC  X(040).
120516       15 PO-DRKI2918-G10VBESTMANAGKZ      PIC  X(001).
             15 PO-DRKI2918-G10VBUENDKZ          PIC  X(0001).
             15 PO-DRKI2918-G10VCGESELL          PIC  X(0050).
120516       15 PO-DRKI2918-G10VCOCDATUM         PIC  X(0010).
120516       15 PO-DRKI2918-G10VCOCKZ            PIC  X(0001).
             15 PO-DRKI2918-G10VCPBRDNR          PIC  X(0050).
             15 PO-DRKI2918-G10VCVORNR           PIC  X(0017).
             15 PO-DRKI2918-G10VERSTDATUM        PIC  X(010).
             15 PO-DRKI2918-G10VERSTZEIT         PIC S9(9) COMP.
             15 PO-DRKI2918-G10VFLGDAT           PIC  X(010).
             15 PO-DRKI2918-G10VGBSALTXT         PIC  X(0050).
             15 PO-DRKI2918-G10VHERKUNFT         PIC  X(0003).
             15 PO-DRKI2918-G10VINFO             PIC  X(0080).
             15 PO-DRKI2918-G10VINKBSA           PIC  X(0006).
             15 PO-DRKI2918-G10VINKELBTRB        PIC S9(8)V9(2) COMP-3.
             15 PO-DRKI2918-G10VINKELBTRN        PIC S9(8)V9(2) COMP-3.
             15 PO-DRKI2918-G10VINKFBBTR         PIC S9(8)V9(2) COMP-3.
             15 PO-DRKI2918-G10VINKFBSUMME       PIC S9(9) COMP.
             15 PO-DRKI2918-G10VINKFORM          PIC  X(0030).
             15 PO-DRKI2918-G10VINKJGBTR         PIC S9(10)V9(2) COMP-3.
             15 PO-DRKI2918-G10VINKJGNBTR        PIC S9(8)V9(2) COMP-3.
             15 PO-DRKI2918-G10VINKKDBON         PIC  X(0026).
             15 PO-DRKI2918-G10VINKLFZRAB        PIC S9(4) COMP.
             15 PO-DRKI2918-G10VINKLSTAGT        PIC  X(010).
             15 PO-DRKI2918-G10VINKLSTBEWG       PIC  X(0061).
             15 PO-DRKI2918-G10VINKLSTSYN        PIC  X(010).
             15 PO-DRKI2918-G10VINKMHNDAT        PIC  X(010).
             15 PO-DRKI2918-G10VINKMHNSTF        PIC  X(0025).
             15 PO-DRKI2918-G10VINKOSTRAB        PIC S9(4) COMP.
             15 PO-DRKI2918-G10VINKRATZU         PIC S9(4) COMP.
             15 PO-DRKI2918-G10VINKRBTR          PIC S9(7)V9(2) COMP-3.
             15 PO-DRKI2918-G10VMANDANT          PIC S9(9) COMP.
             15 PO-DRKI2918-G10VNINKBTR          PIC S9(10)V9(2) COMP-3.
             15 PO-DRKI2918-G10VNVDNR            PIC S9(9) COMP.
             15 PO-DRKI2918-G10VPIN              PIC  X(0009).
             15 PO-DRKI2918-G10VPIN-PRUEF        PIC  X(0002).
120516       15 PO-DRKI2918-G10VPRKOMBIBEZ       PIC  X(0050).
             15 PO-DRKI2918-G10VQUITTNR          PIC  X(0015).
             15 PO-DRKI2918-G10VSPARTE           PIC  X(0009).
             15 PO-DRKI2918-G10VSTATUS           PIC  X(0009).
             15 PO-DRKI2918-G10VSUMME            PIC S9(10)V COMP-3.
             15 PO-DRKI2918-G10VURWAEHR          PIC  X(0009).
             15 PO-DRKI2918-G10VVART             PIC S9(9) COMP.
             15 PO-DRKI2918-G10VVDNAME           PIC  X(0020).
             15 PO-DRKI2918-G10VVERSRISIKO       PIC  X(0023).
             15 PO-DRKI2918-G10VZAHLWEISE        PIC  X(0009).
             15 PO-DRKI2918-G10VZEICHNUNG        PIC  X(0025).
             15 PO-DRKI2918-G10V1MERKM           PIC  X(0020).
             15 PO-DRKI2918-G10V1MNR             PIC  X(0010).
             15 PO-DRKI2918-G10V2MERKM           PIC  X(0020).
             15 PO-DRKI2918-G10V2MNR             PIC  X(0010).
050816       15 PO-DRKI2918-G10VKUNDENIDSMH      PIC  X(0010).
010217       10 PO-DRKI2918-G10VFLEXNVGVKZ       PIC  X(0001).
010217       10 PO-DRKI2918-G10VFLEXNVGVNL       PIC S9(9)V9(2) COMP-3.
010217       10 PO-DRKI2918-G10VFLEXNVSPKZ       PIC  X(0001).
010217       10 PO-DRKI2918-G10VFLEXNVSPNL       PIC S9(9)V9(2) COMP-3.
 
      ***** Ende Copy-Member DRKI2918 *****************************
 
 
 

