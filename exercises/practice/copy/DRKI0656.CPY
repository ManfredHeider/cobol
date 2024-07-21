      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0656.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0656.CPv  $
      *
      *   Rev 2.0   Apr 15 1999 11:37:14   T13EX50
      *Produktionsübergabe - CKV
      *
      *   Rev 1.1   Mar 25 1999 10:35:50   T13EX50
      *Erste Version 
      *
      *   Rev 1.0   25 Aug 1998 09:25:02   P13SP99
      *Initial revision.
      *****************************************************************
      *    COPY-MEMBER: DRKI0656                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0656                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  07.10.98/15.49.11
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
      *    FÜR DIE FUNKTION DRKI0656
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0656.
           05  PI-DRKI0656-KEY.


             10 PI-DRKI0656-KRTVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0656-KRTVERTRAGSID   PIC X(13).


           05  PI-DRKI0656-DATEN.


             10 PI-DRKI0656-KRTBEITRAGVERZ   PIC S9(2)V9(1) COMP-3.
             10 PI-DRKI0656-KRTGESCHAEFTSST   PIC X(20).
             10 PI-DRKI0656-KRTGESCHAEFTSSTID   PIC S9(4) COMP.
             10 PI-DRKI0656-KRTPERSONENNUMMER   PIC S9(4) COMP.


      *
       01  PO-DRKI0656.
           05  PO-DRKI0656-RC                 PIC  X(003).
               88 PO-DRKI0656-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0656-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
