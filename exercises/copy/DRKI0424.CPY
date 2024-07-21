      * I13PC18   Stand: 27/08/98    13:06:39
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0424.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI0424.CPv  $
      *
      *   Rev 2.1   02 Feb 2009 10:17:58   t13ex27
      *AKI00768: CR 22257 - Bearbeitungskennzeichen Rechenwerte-DB-
      *Änderung
      *
      *   Rev 2.0   Feb 27 1999 12:57:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   08 Feb 1999 17:06:30   I13PC18
      *FUNKTION UPDATE FUER ZTKI0357
      *
      *   Rev 1.0   28 Jul 1998 09:52:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*


      *  Update LRW

      *****************************************************************
      *    COPY-MEMBER: DRKI0424                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0424                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  20.01.2009/11:28:37
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
      *    FÜR DIE FUNKTION DRKI0424
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0424.
           05  PI-DRKI0424-KEY.


             10 PI-DRKI0424-VERVERTRAGSID   PIC X(13).


           05  PI-DRKI0424-DATEN.


             10 PI-DRKI0424-VERGESAMTBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0424-N-VRGSMTBTRG   PIC S9(4) COMP.
             10 PI-DRKI0424-VERGESBEITRBKZ   PIC X(1).


      *
       01  PO-DRKI0424.
           05  PO-DRKI0424-RC                 PIC  X(003).
               88 PO-DRKI0424-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0424-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
