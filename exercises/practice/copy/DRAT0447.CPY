      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.01.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0447.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0447.CPv  $
      *
      *   Rev 2.2   Jan 28 2009 10:42:36   t13ex14
      *AKI00779: Masch. DTA-Kontrolle Badenia täglich
      *
      *   Rev 2.0   Jan 26 2009 09:20:42   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0447                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0447                    *
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
      *    FÜR DIE FUNKTION DRAT0447
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRAT0447.
           05  PI-DRAT0447-KEY.


             10 PI-DRAT0447-DTADATEINUMMER   PIC S9(4) COMP.
             10 PI-DRAT0447-DTAGRUPPE   PIC S9(4) COMP.
             10 PI-DRAT0447-DTALFN   PIC S9(9) COMP.
             10 PI-DRAT0447-DTAMANDANTNR   PIC S9(9) COMP.
             10 PI-DRAT0447-DTAPARTGESID   PIC S9(4) COMP.
             
      *
       01  PO-DRAT0447.
           05  PO-DRAT0447-RC                 PIC  X(003).
               88 PO-DRAT0447-RC-OK                        VALUE 'OK '.
               88 PO-DRAT0447-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRAT0447-DATEN.


             10 PO-DRAT0447-DTADATEI   PIC X(36).
             10 PO-DRAT0447-DTAERSTERSATZ.
                15 PO-DRAT0447-DTAERSTERSATZ-LEN   PIC S9(4) COMP.
                15 PO-DRAT0447-DTAERSTERSATZ-TEXT   PIC X(1000).
                
             10 PO-DRAT0447-DTAGDGSTATUS   PIC X(1).
             10 PO-DRAT0447-DTAGEN   PIC X(8).
             10 PO-DRAT0447-DTAJOBNAME   PIC X(8).
             10 PO-DRAT0447-DTAKATALOGDATUM   PIC X(10).
             10 PO-DRAT0447-DTAPARTNERDATUM   PIC X(10).
             10 PO-DRAT0447-DTASATZANZAHL   PIC S9(9) COMP.
             

      *
      ******* ENDE COPY-MEMBER **************************************
