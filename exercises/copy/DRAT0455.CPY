      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.01.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0455.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0455.CPv  $
      *
      *   Rev 2.1   Feb 24 2009 13:44:42   t13ex14
      *AKI00787: Maschinelle Info über Aktualisierungsjobs - OZP-Ob
      *jekte
      *
      *   Rev 2.0   Jan 26 2009 14:56:56   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0455                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0455                    *
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
      *    FÜR DIE FUNKTION DRAT0455
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRAT0455.
           05  PI-DRAT0455-KEY.


             10 PI-DRAT0455-ABKJOBNAME   PIC X(8).
             10 PI-DRAT0455-ABKLFDNR   PIC S9(9) COMP.
             10 PI-DRAT0455-ABKMANDANTNR   PIC S9(9) COMP.
             10 PI-DRAT0455-ABKPARTGESID   PIC S9(4) COMP.
             

           05  PI-DRAT0455-DATEN.


             10 PI-DRAT0455-ABKBEGABLAUF   PIC X(26).
             10 PI-DRAT0455-ABKBEGAKTU   PIC X(26).
             10 PI-DRAT0455-ABKBEGEINARB   PIC X(26).
             10 PI-DRAT0455-ABKENDAKTU   PIC X(26).
             10 PI-DRAT0455-ABKENDEINARB   PIC X(26).
             10 PI-DRAT0455-ABKKATALOGDATUM   PIC X(10).
             10 PI-DRAT0455-ABKPARTNERDATUM   PIC X(10).
             

      *
       01  PO-DRAT0455.
           05  PO-DRAT0455-RC                 PIC  X(003).
               88 PO-DRAT0455-RC-OK                        VALUE 'OK '.
               88 PO-DRAT0455-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
