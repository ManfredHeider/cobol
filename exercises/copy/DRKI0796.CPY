      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0796.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0796.CPv  $
      *
      *   Rev 2.1   Apr 08 1999 16:24:54   T13EX51
      *AKI00021
      *
      *   Rev 2.0   Mar 01 1999 15:07:46   T13EX51
      *Neue Revision AKI000001
      *
      *   Rev 1.0   06 Oct 1998 15:41:16   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0796                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0796                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  20.01.99/16.53.45
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
      *    FÜR DIE FUNKTION DRKI0796
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0796.
           05  PI-DRKI0796-KEY.


             10 PI-DRKI0796-VMLVERTRAGSID   PIC X(13).

      *
       01  PO-DRKI0796.
           05  PO-DRKI0796-RC                 PIC  X(003).
               88 PO-DRKI0796-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0796-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI0796-DATEN.


             10 PO-DRKI0796-VMLARBEITGEBZUSCH   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI0796-VMLARBEITNEHMSPAR   PIC X(01).
             10 PO-DRKI0796-VMLARBGEBZUSCHWKZ   PIC X(01).
             10 PO-DRKI0796-VMLARBSPARZULDAT   PIC X(10).
             10 PO-DRKI0796-VMLARBSPARZULSUM   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI0796-VMLARBSPZULSUMWKZ   PIC X(01).
             10 PO-DRKI0796-VMLBEITRGESWKZ   PIC X(01).
             10 PO-DRKI0796-VMLENDESTEUERLFZ   PIC X(10).
             10 PO-DRKI0796-VMLVLBEITRAGGES   PIC S9(9)V9(2) COMP-3.


      *
      ******* ENDE COPY-MEMBER **************************************
