      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.04.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1884.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI1884.CPv  $
      *
      *   Rev 2.4   Jul 21 2009 13:33:12   t13ex14
      *AKI00817: Ausweis von Zusatzinformationen zu Advocard; OZP-O
      *bjekte
      *
      *   Rev 2.0   Apr 19 2006 11:17:54   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1884                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1884                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  20.05.2009/11:49:47
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
      *    FÜR DIE FUNKTION DRKI1884
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1884.
           05  PI-DRKI1884-KEY.


             10 PI-DRKI1884-SATVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1884-SATVERTRAGSID   PIC X(13).
             

           05  PI-DRKI1884-DATEN.


             10 PI-DRKI1884-SATDAUERNACHLASS   PIC X(1).
             10 PI-DRKI1884-SATDIFFABLAUF   PIC X(10).
             10 PI-DRKI1884-SATDIFFGES   PIC X(90).
             10 PI-DRKI1884-SATDIFFVNR   PIC X(40).
             10 PI-DRKI1884-SATEEGFKOSTENG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1884-NI-SATEEGFKOSTENG   PIC S9(4) COMP.
             10 PI-DRKI1884-SATEEGFKOSTENGWKZ   PIC X(1).
             10 PI-DRKI1884-SATEGKUENSTGS   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1884-NI-SATEGKUENSTGS   PIC S9(4) COMP.
             10 PI-DRKI1884-SATEGKUENSTGSKZ   PIC X(1).
             10 PI-DRKI1884-SATEGPRIVATGH   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1884-NI-SATEGPRIVATGH   PIC S9(4) COMP.
             10 PI-DRKI1884-SATEGPRIVATGHWKZ   PIC X(1).
             10 PI-DRKI1884-SATGLASBERECHID   PIC S9(4) COMP.
             10 PI-DRKI1884-NI-SATGLASBERECHID   PIC S9(4) COMP.
             10 PI-DRKI1884-SATGLASBERECHWERT   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1884-N-STGLSBRCHWRT   PIC S9(4) COMP.
             10 PI-DRKI1884-SATGLASBERWERTWKZ   PIC X(01).
             10 PI-DRKI1884-SATHINWAUFSELBSTB   PIC X(01).
             10 PI-DRKI1884-SATJAHRBEITRWKZ   PIC X(01).
             10 PI-DRKI1884-SATJAHRESBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1884-N-STJHRSBTRG   PIC S9(4) COMP.
             10 PI-DRKI1884-SATKUNDENBONUS   PIC X(1).
             10 PI-DRKI1884-SATKZDIFFKUEND   PIC X(1).
             10 PI-DRKI1884-SATKZDIFFWG   PIC X(1).
             10 PI-DRKI1884-SATOEFFDIENSTNL   PIC X(1).
             10 PI-DRKI1884-SATPRODUKTTEXTFV   PIC X(20).
             10 PI-DRKI1884-SATPRODVARIANTE   PIC X(30).
             10 PI-DRKI1884-SATRISKANSCHRORT   PIC X(30).
             10 PI-DRKI1884-SATRISKANSCHRPLZ   PIC X(6).
             10 PI-DRKI1884-SATRISKANSCHRSTR   PIC X(45).
             10 PI-DRKI1884-SATSEBSTBETWKZ   PIC X(01).
             10 PI-DRKI1884-SATSELBSTBETEIL   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1884-NI-SATSELBSTBETEIL   PIC S9(4) COMP.
             10 PI-DRKI1884-SATSONSTIGE   PIC X(30).
             10 PI-DRKI1884-SATSUBTYPE   PIC X(01).
             10 PI-DRKI1884-SATTARIFSB   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1884-NI-SATTARIFSB   PIC S9(4) COMP.
             10 PI-DRKI1884-SATTARIFSBWKZ   PIC X(1).
             10 PI-DRKI1884-SATVERSLSTID   PIC S9(4) COMP.
             10 PI-DRKI1884-NI-SATVERSLSTID   PIC S9(4) COMP.
             10 PI-DRKI1884-SATVSPNACHLASS   PIC X(1).
             10 PI-DRKI1884-SATZUKNETTOJBR   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1884-NI-SATZUKNETTOJBR   PIC S9(4) COMP.
             10 PI-DRKI1884-SATZUKNETTOJBRWKZ   PIC X(1).
             10 PI-DRKI1884-SATZWEIGSCHLUESSEL   PIC X(2).
             10 PI-DRKI1884-SATZWEIGTEXT   PIC X(3).
             

      *
       01  PO-DRKI1884.
           05  PO-DRKI1884-RC                 PIC  X(003).
               88 PO-DRKI1884-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1884-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
