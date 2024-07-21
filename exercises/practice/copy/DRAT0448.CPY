      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.01.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0448.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0448.CPv  $
      *
      *   Rev 2.2   Jan 28 2009 10:42:40   t13ex14
      *AKI00779: Masch. DTA-Kontrolle Badenia täglich
      *
      *   Rev 2.0   Jan 26 2009 09:20:46   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0448                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0448                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  27.01.2009/13:36:12
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
      *    FÜR DIE FUNKTION DRAT0448
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRAT0448.
           05  PI-DRAT0448-KEY.


             10 PI-DRAT0448-DTADATEINUMMER   PIC S9(4) COMP.
             10 PI-DRAT0448-DTAGRUPPE   PIC S9(4) COMP.
      *      DTALFN wird mittels "next value"-Funktion befüllt
      *      10 PI-DRAT0448-DTALFN   PIC S9(9) COMP.
             10 PI-DRAT0448-DTAMANDANTNR   PIC S9(9) COMP.
             10 PI-DRAT0448-DTAPARTGESID   PIC S9(4) COMP.
             

           05  PI-DRAT0448-DATEN.


             10 PI-DRAT0448-DTADATEI   PIC X(36).
             10 PI-DRAT0448-DTAERSTERSATZ.
                15 PI-DRAT0448-DTAERSTERSATZ-LEN   PIC S9(4) COMP.
                15 PI-DRAT0448-DTAERSTERSATZ-TEXT   PIC X(1000).
                
             10 PI-DRAT0448-DTAGDGSTATUS   PIC X(1).
             10 PI-DRAT0448-DTAGEN   PIC X(8).
             10 PI-DRAT0448-DTAJOBNAME   PIC X(8).
             10 PI-DRAT0448-DTAKATALOGDATUM   PIC X(10).
             10 PI-DRAT0448-DTAPARTNERDATUM   PIC X(10).
             10 PI-DRAT0448-DTASATZANZAHL   PIC S9(9) COMP.
             
      *
       01  PO-DRAT0448.
           05  PO-DRAT0448-RC                 PIC  X(003).
               88 PO-DRAT0448-RC-OK                        VALUE 'OK '.
               88 PO-DRAT0448-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
