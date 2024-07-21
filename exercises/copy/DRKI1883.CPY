      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.04.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1883.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI1883.CPv  $
      *
      *   Rev 2.4   Jul 21 2009 13:33:06   t13ex14
      *AKI00817: Ausweis von Zusatzinformationen zu Advocard; OZP-O
      *bjekte
      *
      *   Rev 2.0   Apr 19 2006 11:17:30   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1883                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1883                    *
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
      *    FÜR DIE FUNKTION DRKI1883
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1883.
           05  PI-DRKI1883-KEY.


             10 PI-DRKI1883-SATVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1883-SATVERTRAGSID   PIC X(13).
             
      *
       01  PO-DRKI1883.
           05  PO-DRKI1883-RC                 PIC  X(003).
               88 PO-DRKI1883-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1883-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1883-DATEN.


             10 PO-DRKI1883-SATDAUERNACHLASS   PIC X(1).
             10 PO-DRKI1883-SATDIFFABLAUF   PIC X(10).
             10 PO-DRKI1883-SATDIFFGES   PIC X(90).
             10 PO-DRKI1883-SATDIFFVNR   PIC X(40).
             10 PO-DRKI1883-SATEEGFKOSTENG   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1883-NI-SATEEGFKOSTENG   PIC S9(4) COMP.
             10 PO-DRKI1883-SATEEGFKOSTENGWKZ   PIC X(1).
             10 PO-DRKI1883-SATEGKUENSTGS   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1883-NI-SATEGKUENSTGS   PIC S9(4) COMP.
             10 PO-DRKI1883-SATEGKUENSTGSKZ   PIC X(1).
             10 PO-DRKI1883-SATEGPRIVATGH   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1883-NI-SATEGPRIVATGH   PIC S9(4) COMP.
             10 PO-DRKI1883-SATEGPRIVATGHWKZ   PIC X(1).
             10 PO-DRKI1883-SATGLASBERECHID   PIC S9(4) COMP.
             10 PO-DRKI1883-NI-SATGLASBERECHID   PIC S9(4) COMP.
             10 PO-DRKI1883-SATGLASBERECHWERT   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1883-N-STGLSBRCHWRT   PIC S9(4) COMP.
             10 PO-DRKI1883-SATGLASBERWERTWKZ   PIC X(01).
             10 PO-DRKI1883-SATHINWAUFSELBSTB   PIC X(01).
             10 PO-DRKI1883-SATJAHRBEITRWKZ   PIC X(01).
             10 PO-DRKI1883-SATJAHRESBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1883-N-STJHRSBTRG   PIC S9(4) COMP.
             10 PO-DRKI1883-SATKUNDENBONUS   PIC X(1).
             10 PO-DRKI1883-SATKZDIFFKUEND   PIC X(1).
             10 PO-DRKI1883-SATKZDIFFWG   PIC X(1).
             10 PO-DRKI1883-SATOEFFDIENSTNL   PIC X(1).
             10 PO-DRKI1883-SATPRODUKTTEXTFV   PIC X(20).
             10 PO-DRKI1883-SATPRODVARIANTE   PIC X(30).
             10 PO-DRKI1883-SATRISKANSCHRORT   PIC X(30).
             10 PO-DRKI1883-SATRISKANSCHRPLZ   PIC X(6).
             10 PO-DRKI1883-SATRISKANSCHRSTR   PIC X(45).
             10 PO-DRKI1883-SATSEBSTBETWKZ   PIC X(01).
             10 PO-DRKI1883-SATSELBSTBETEIL   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1883-NI-SATSELBSTBETEIL   PIC S9(4) COMP.
             10 PO-DRKI1883-SATSONSTIGE   PIC X(30).
             10 PO-DRKI1883-SATSUBTYPE   PIC X(01).
             10 PO-DRKI1883-SATTARIFSB   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1883-NI-SATTARIFSB   PIC S9(4) COMP.
             10 PO-DRKI1883-SATTARIFSBWKZ   PIC X(1).
             10 PO-DRKI1883-SATVERSLSTID   PIC S9(4) COMP.
             10 PO-DRKI1883-NI-SATVERSLSTID   PIC S9(4) COMP.
             10 PO-DRKI1883-SATVSPNACHLASS   PIC X(1).
             10 PO-DRKI1883-SATZUKNETTOJBR   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1883-NI-SATZUKNETTOJBR   PIC S9(4) COMP.
             10 PO-DRKI1883-SATZUKNETTOJBRWKZ   PIC X(1).
             10 PO-DRKI1883-SATZWEIGSCHLUESSEL   PIC X(2).
             10 PO-DRKI1883-SATZWEIGTEXT   PIC X(3).
             

      *
      ******* ENDE COPY-MEMBER **************************************
