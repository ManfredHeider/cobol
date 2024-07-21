      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.11.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1999.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1999.CPv  $
      *
      *   Rev 2.1   17 Nov 2006 13:23:12   t13ex39
      *FKI01058: UNFVSKHTREHAWKZ falsches Format
      *
      *   Rev 2.0   16 Nov 2006 17:38:18   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1999                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1999                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  16.11.2006/16:48:58
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FKI01058!17.11.06! 02 ! SELECT                    ! WHO      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI1999
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01 PI-DRKI1999.
          05 PI-DRKI1999-PROGRAMM                 PIC  X(008).
          05 PI-DRKI1999-UFKT                     PIC  9(002).
      *
       01 PO-DRKI1999.

          05 PO-DRKI1999-RC                       PIC  X(003).
             88 PO-DRKI1999-RC-OK                    VALUE 'OK '.
             88 PO-DRKI1999-RC-NOT-FOUND             VALUE 'NFD'.
             88 PO-DRKI1999-RC-EOF                   VALUE 'EOF'.

      *
           05  PO-DRKI1999-KEY.


             10 PO-DRKI1999-UNFVERSTARLFDNR   PIC S9(4) COMP.
             10 PO-DRKI1999-UNFVERTRAGSID   PIC X(13).
             
           05  PO-DRKI1999-DATEN.

             10 PO-DRKI1999-UNFAKTMEHRLINVWKZ   PIC X(1).
             10 PO-DRKI1999-UNFAKTMEHRLLINV   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1999-UNFAKTPERSONKZ   PIC X(1).
             10 PO-DRKI1999-UNFAKTSCHMERZG   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1999-UNFAKTSCHMERZGWKZ   PIC X(1).
             10 PO-DRKI1999-UNFBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1999-UNFBEITRAGWKZ   PIC X(1).
             10 PO-DRKI1999-UNFBERGUNGBETRAG   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1999-UNFBERGUNGBETRWKZ   PIC X(1).
             10 PO-DRKI1999-UNFBEZUGSRECHT   PIC X(40).
             10 PO-DRKI1999-UNFDIFFABLAUF   PIC X(10).
             10 PO-DRKI1999-UNFDIFFGES   PIC X(90).
             10 PO-DRKI1999-UNFDIFFVNR   PIC X(40).
             10 PO-DRKI1999-UNFFRABFPRINVWKZ   PIC X(1).
             10 PO-DRKI1999-UNFGRUNDSUMME   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1999-UNFGRUNDSUMWKZ   PIC X(1).
             10 PO-DRKI1999-UNFHAUSHALTSGELD   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1999-UNFHHGELDWKZ   PIC X(1).
             10 PO-DRKI1999-UNFHOEHEPROGRESS   PIC S9(4)V9(2) COMP-3.
             10 PO-DRKI1999-UNFHOEHEPROGWKZ   PIC X(1).
             10 PO-DRKI1999-UNFKENNZEICHENBG   PIC X(1).
             10 PO-DRKI1999-UNFKOSMBETRWKZ   PIC X(1).
             10 PO-DRKI1999-UNFKOSMETIKBETRAG   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1999-UNFKURBETRAG   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1999-UNFKURBETRAGWKZ   PIC X(1).
             10 PO-DRKI1999-UNFKZDIFFKUEND   PIC X(1).
             10 PO-DRKI1999-UNFKZDIFFWG   PIC X(1).
             10 PO-DRKI1999-UNFLVASS   PIC X(20).
             10 PO-DRKI1999-UNFLVASSKZ   PIC X(1).
             10 PO-DRKI1999-UNFLVKHTR   PIC X(15).
             10 PO-DRKI1999-UNFRABFUENFPRINV   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1999-UNFSCHMGELDKB   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1999-UNFSCHMGELDKBWKZ   PIC X(1).
             10 PO-DRKI1999-UNFSCHULAUSFGELD   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1999-UNFSCHULAUSFGWKZ   PIC X(1).
             10 PO-DRKI1999-UNFSOFORTLSCHWV   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1999-UNFSOFORTLSCHWVWKZ   PIC X(1).
             10 PO-DRKI1999-UNFSUMMENKOMB   PIC X(10).
             10 PO-DRKI1999-UNFUEBERGLSTBETR   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1999-UNFUEBERGLSTBWKZ   PIC X(1).
             10 PO-DRKI1999-UNFVEREINBDYNPROZ   PIC S9(3)V9(2) COMP-3.
             10 PO-DRKI1999-UNFVERSSUMGENGELD   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1999-UNFVERSSUMINVALID   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1999-UNFVERSSUMKRANKTG   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1999-UNFVERSSUMTAGGELD   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1999-UNFVERSSUMTOD   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1999-UNFVSGENGELDWKZ   PIC X(1).
             10 PO-DRKI1999-UNFVSINVALIDWKZ   PIC X(1).
             10 PO-DRKI1999-UNFVSKHTREHA   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1999-UNFVSKHTREHAWKZ   PIC X(1).
             10 PO-DRKI1999-UNFVSKRANKTGWKZ   PIC X(1).
             10 PO-DRKI1999-UNFVSSCHMOSOA   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1999-UNFVSSCHMOSOAWKZ   PIC X(1).
             10 PO-DRKI1999-UNFVSTAGGELDWKZ   PIC X(1).
             10 PO-DRKI1999-UNFVSTODWKZ   PIC X(1).
             10 PO-DRKI1999-UNFZUKBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1999-UNFZUKBEITRAGWKZ   PIC X(1).
             10 PO-DRKI1999-UNFZWEIGSCHLUESSEL   PIC X(2).

      *
      ******* ENDE COPY-MEMBER **************************************
