      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.06.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2021 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2914.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI2914.CPv  $
      *
      *   Rev 2.7   Feb 05 2021 09:46:38   t13ae31
      *AKI01827: G10 SUH VSPKZ DB2
      *
      *   Rev 2.6   Mar 01 2017 10:50:04   t13ae31
      *AKI01493: SUH neue Felder FLEXN OZP
      *
      *   Rev 2.5   Sep 19 2016 07:37:18   t13ae31
      *AKI01460: SUH SMART-HOME OZP KSDS 468 Vertrag
      *
      *   Rev 2.4   Jun 23 2016 08:44:22   t13ae31
      *AKI01438: G10 SUH Angleichung DTA OZP ZT/ZM
      *
      *   Rev 2.3   Mar 30 2016 08:13:06   t13ae31
      *AKI01405: COC  G10 SUH OZP KSDS
      *
      *   Rev 2.2   Oct 08 2014 13:43:28   t13ex78
      *AKI01290: SUH KSDS in DB/2 - Copy-Strecken für Neuprogramme
      *und Änderungen
      *
      *   Rev 2.1   Sep 01 2014 11:28:10   t13ex78
      *AKI01290: SUH KSDS in DB/2 - Standard-OZP für TCKI0468 G10KS
      *DSSUHSA400
      *
      *   Rev 2.0   Jun 4 2014 08:12:37 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2914                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2914                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         03.02.2021 / 09:27:07 / T13AE31
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2914
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2914.
           05  PI-DRKI2914-KEY.

ANF   *        %M_PI_KEY%
             10 PI-DRKI2914-G10VVERTRAGSNR   PIC  X(0015).
END   *

           05  PI-DRKI2914-NI.

ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI2914-NI-G10VAKTIONERG    PIC S9(4) COMP.
             15 PI-DRKI2914-NI-G10VBESCHRBMKZ   PIC S9(4) COMP.
             15 PI-DRKI2914-NI-G10VBESTMANAGKZ  PIC S9(4) COMP.
             15 PI-DRKI2914-NI-G10VCOCDATUM     PIC S9(4) COMP.
             15 PI-DRKI2914-NI-G10VCOCKZ        PIC S9(4) COMP.
             15 PI-DRKI2914-NI-G10VFLEXNVGVKZ   PIC S9(4) COMP.
             15 PI-DRKI2914-NI-G10VFLEXNVGVNL   PIC S9(4) COMP.
             15 PI-DRKI2914-NI-G10VFLEXNVSPKZ   PIC S9(4) COMP.
             15 PI-DRKI2914-NI-G10VFLEXNVSPNL   PIC S9(4) COMP.
             15 PI-DRKI2914-NI-G10VKUNDENIDSMH  PIC S9(4) COMP.
             15 PI-DRKI2914-NI-G10VPRKOMBIBEZ   PIC S9(4) COMP.
END   *

           05  PI-DRKI2914-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI2914-G10VABLAUF   PIC  X(010).
             10 PI-DRKI2914-G10VABSCHLVBNR   PIC S9(9) COMP.
             10 PI-DRKI2914-G10VAENDDAT   PIC  X(010).
             10 PI-DRKI2914-G10VAENDGRD   PIC  X(0050).
             10 PI-DRKI2914-G10VAENDKZ   PIC  X(0002).
             10 PI-DRKI2914-G10VAFBPROZ   PIC S9(2)V9(3) COMP-3.
             10 PI-DRKI2914-G10VAKTIONERG   PIC  X(0003).
             10 PI-DRKI2914-G10VANTNR   PIC  X(0018).
             10 PI-DRKI2914-G10VBEGINN   PIC  X(010).
             10 PI-DRKI2914-G10VBESCHRBMKZ   PIC  X(0040).
             10 PI-DRKI2914-G10VBESTMANAGKZ   PIC  X(0001).
             10 PI-DRKI2914-G10VBUENDKZ   PIC  X(0001).
             10 PI-DRKI2914-G10VCGESELL   PIC  X(0050).
             10 PI-DRKI2914-G10VCOCDATUM   PIC  X(010).
             10 PI-DRKI2914-G10VCOCKZ   PIC  X(0001).
             10 PI-DRKI2914-G10VCPBRDNR   PIC  X(0050).
             10 PI-DRKI2914-G10VCVORNR   PIC  X(0017).
             10 PI-DRKI2914-G10VERSTDATUM   PIC  X(010).
             10 PI-DRKI2914-G10VERSTZEIT   PIC S9(9) COMP.
             10 PI-DRKI2914-G10VFLEXNVGVKZ   PIC  X(0001).
             10 PI-DRKI2914-G10VFLEXNVGVNL   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2914-G10VFLEXNVSPKZ   PIC  X(0001).
             10 PI-DRKI2914-G10VFLEXNVSPNL   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2914-G10VFLGDAT   PIC  X(010).
             10 PI-DRKI2914-G10VGBSALTXT   PIC  X(0050).
             10 PI-DRKI2914-G10VHERKUNFT   PIC  X(0003).
             10 PI-DRKI2914-G10VINFO   PIC  X(0080).
             10 PI-DRKI2914-G10VINKBSA   PIC  X(0006).
             10 PI-DRKI2914-G10VINKELBTRB   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2914-G10VINKELBTRN   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2914-G10VINKFBBTR   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2914-G10VINKFBSUMME   PIC S9(9) COMP.
             10 PI-DRKI2914-G10VINKFORM   PIC  X(0030).
             10 PI-DRKI2914-G10VINKJGBTR   PIC S9(10)V9(2) COMP-3.
             10 PI-DRKI2914-G10VINKJGNBTR   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2914-G10VINKKDBON   PIC  X(0026).
             10 PI-DRKI2914-G10VINKLFZRAB   PIC S9(4) COMP.
             10 PI-DRKI2914-G10VINKLSTAGT   PIC  X(010).
             10 PI-DRKI2914-G10VINKLSTBEWG   PIC  X(0061).
             10 PI-DRKI2914-G10VINKLSTSYN   PIC  X(010).
             10 PI-DRKI2914-G10VINKMHNDAT   PIC  X(010).
             10 PI-DRKI2914-G10VINKMHNSTF   PIC  X(0025).
             10 PI-DRKI2914-G10VINKOSTRAB   PIC S9(4) COMP.
             10 PI-DRKI2914-G10VINKRATZU   PIC S9(4) COMP.
             10 PI-DRKI2914-G10VINKRBTR   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI2914-G10VKUNDENIDSMH   PIC  X(0010).
             10 PI-DRKI2914-G10VMANDANT   PIC S9(9) COMP.
             10 PI-DRKI2914-G10VNINKBTR   PIC S9(10)V9(2) COMP-3.
             10 PI-DRKI2914-G10VNVDNR   PIC S9(9) COMP.
             10 PI-DRKI2914-G10VPIN   PIC  X(0009).
             10 PI-DRKI2914-G10VPIN-PRUEF   PIC  X(0002).
             10 PI-DRKI2914-G10VPRKOMBIBEZ   PIC  X(0050).
             10 PI-DRKI2914-G10VQUITTNR   PIC  X(0015).
             10 PI-DRKI2914-G10VSPARTE   PIC  X(0009).
             10 PI-DRKI2914-G10VSTATUS   PIC  X(0009).
             10 PI-DRKI2914-G10VSUMME   PIC S9(10)V COMP-3.
             10 PI-DRKI2914-G10VURWAEHR   PIC  X(0009).
             10 PI-DRKI2914-G10VVART   PIC S9(9) COMP.
             10 PI-DRKI2914-G10VVDNAME   PIC  X(0020).
             10 PI-DRKI2914-G10VVERSRISIKO   PIC  X(0023).
             10 PI-DRKI2914-G10VVSPKZ   PIC  X(0003).
             10 PI-DRKI2914-G10VZAHLWEISE   PIC  X(0009).
             10 PI-DRKI2914-G10VZEICHNUNG   PIC  X(0025).
             10 PI-DRKI2914-G10V1MERKM   PIC  X(0020).
             10 PI-DRKI2914-G10V1MNR   PIC  X(0010).
             10 PI-DRKI2914-G10V2MERKM   PIC  X(0020).
             10 PI-DRKI2914-G10V2MNR   PIC  X(0010).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2914-NI1.
           05 PI-DRKI2914-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 11.
       01  PI-DRKI2914-NI7.
           05 PI-DRKI2914-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 11.

      *
       01  PO-DRKI2914.
           05  PO-DRKI2914-RC                     PIC  X(003).
               88 PO-DRKI2914-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2914-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

