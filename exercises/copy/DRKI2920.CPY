      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.06.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2920.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI2920.CPv  $
      *
      *   Rev 2.6   Jun 23 2016 09:09:28   t13ae31
      *AKI01438: G10 SUH Angleichung DTA OZP ZX
      *
      *   Rev 2.5   Oct 08 2014 13:44:22   t13ex78
      *AKI01290: SUH KSDS in DB/2 - Copy-Strecken für Neuprogramme
      *und Änderungen
      *
      *   Rev 2.4   Sep 23 2014 14:13:54   t13ex78
      *AKI01290: SUH KSDS in DB/2 - List-OZP für TCKI0469 G10KSDSSU
      *HSA401
      *
      *   Rev 2.3   Sep 01 2014 13:08:24   t13ex78
      *AKI01290: SUH KSDS in DB/2 - List-OZP für TCKI0469 G10KSDSSU
      *HSA401
      *
      *   Rev 2.2   Sep 01 2014 12:59:36   t13ex78
      *AKI01290: SUH KSDS in DB/2 - List-OZP für TCKI0469 G10KSDSSU
      *HSA401
      *
      *   Rev 2.1   Sep 01 2014 12:13:32   t13ex78
      *AKI01290: SUH KSDS in DB/2 - List-OZP für TCKI0469 G10KSDSSU
      *HSA401
      *
      *   Rev 2.0   Jun 05 2014 09:01:50   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI2920                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXKI0469                        *
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
      * AKI01438!20.05.16!2.0 ! Überarbeitung             ! P.Hübner *
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
 
       01 PI-DRKI2920.
          05 PI-DRKI2920-PROGRAMM                PIC  X(008).
          05 PI-DRKI2920-UFKT-CODE               PIC  9(002).
          05 PI-DRKI2920-DATEN.
             10 PI-DRKI2920-G10EZVVERTRAGSNR     PIC  X(015).
 
       01 PO-DRKI2920.
 
          05 PO-DRKI2920-RC                      PIC  X(003).
             88 PO-DRKI2920-RC-OK                VALUE 'OK '.
             88 PO-DRKI2920-RC-NOT-FOUND         VALUE 'NFD'.
             88 PO-DRKI2920-RC-EOF               VALUE 'EOF'.
 
          05 PO-DRKI2920-DATEN.
             10 PO-DRKI2920-G10EZVHOSTNR   PIC  X(0015).
             10 PO-DRKI2920-G10EZVABLAUF   PIC  X(010).
             10 PO-DRKI2920-G10EZVAENDDAT   PIC  X(010).
             10 PO-DRKI2920-G10EZVAENDGRD   PIC  X(0050).
             10 PO-DRKI2920-G10EZVBEGINN   PIC  X(010).
             10 PO-DRKI2920-G10EZVBRUTJBTR   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI2920-G10EZVLETBEARB   PIC  X(010).
             10 PO-DRKI2920-G10EZVNBTRZW   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI2920-G10EZVNETJBTR   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI2920-G10EZVNVEZBTRG   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI2920-G10EZVPBRDNR   PIC S9(9) COMP.
             10 PO-DRKI2920-G10EZVPRODBEZ   PIC  X(0050).
             10 PO-DRKI2920-G10EZVRABNETTO   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI2920-G10EZVREOBPRDNR   PIC  X(0005).
             10 PO-DRKI2920-G10EZVSPARTE   PIC S9(9) COMP.
             10 PO-DRKI2920-G10EZVSTATUS   PIC  X(0009).
             10 PO-DRKI2920-G10EZVSUAKTBTR   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI2920-G10EZVSUMME   PIC S9(9) COMP.
             10 PO-DRKI2920-G10EZVVART   PIC S9(9) COMP.
             10 PO-DRKI2920-G10EZVZWEIG   PIC  X(0002).
 
      ***** Ende Copy-Member DRKI2920 *****************************
 

