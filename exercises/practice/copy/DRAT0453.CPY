      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.01.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0453.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0453.CPv  $
      *
      *   Rev 2.1   Feb 24 2009 13:44:34   t13ex14
      *AKI00787: Maschinelle Info über Aktualisierungsjobs - OZP-Ob
      *jekte
      *
      *   Rev 2.0   Jan 26 2009 14:56:46   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0453                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0453                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  27.01.2009/15:25:14
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
      *    FÜR DIE FUNKTION DRAT0453
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRAT0453.
           05  PI-DRAT0453-KEY.


             10 PI-DRAT0453-ABKJOBNAME   PIC X(8).
             10 PI-DRAT0453-ABKLFDNR   PIC S9(9) COMP.
             10 PI-DRAT0453-ABKMANDANTNR   PIC S9(9) COMP.
             10 PI-DRAT0453-ABKPARTGESID   PIC S9(4) COMP.
             
      *
       01  PO-DRAT0453.
           05  PO-DRAT0453-RC                 PIC  X(003).
               88 PO-DRAT0453-RC-OK                        VALUE 'OK '.
               88 PO-DRAT0453-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRAT0453-DATEN.


             10 PO-DRAT0453-ABKBEGABLAUF   PIC X(26).
             10 PO-DRAT0453-ABKBEGAKTU   PIC X(26).
             10 PO-DRAT0453-ABKBEGEINARB   PIC X(26).
             10 PO-DRAT0453-ABKENDAKTU   PIC X(26).
             10 PO-DRAT0453-ABKENDEINARB   PIC X(26).
             10 PO-DRAT0453-ABKKATALOGDATUM   PIC X(10).
             10 PO-DRAT0453-ABKPARTNERDATUM   PIC X(10).
             

      *
      ******* ENDE COPY-MEMBER **************************************
