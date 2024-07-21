      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0797.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0797.CPv  $
      *
      *   Rev 2.1   Apr 08 1999 16:25:06   T13EX51
      *AKI00021
      *
      *   Rev 2.0   Mar 01 1999 15:07:48   T13EX51
      *Neue Revision AKI000001
      *
      *   Rev 1.0   06 Oct 1998 15:41:22   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0797                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0797                    *
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
      *    FÜR DIE FUNKTION DRKI0797
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0797.
           05  PI-DRKI0797-KEY.


             10 PI-DRKI0797-VMLVERTRAGSID   PIC X(13).


           05  PI-DRKI0797-DATEN.


             10 PI-DRKI0797-VMLARBEITGEBZUSCH   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0797-VMLARBEITNEHMSPAR   PIC X(01).
             10 PI-DRKI0797-VMLARBGEBZUSCHWKZ   PIC X(01).
             10 PI-DRKI0797-VMLARBSPARZULDAT   PIC X(10).
             10 PI-DRKI0797-VMLARBSPARZULSUM   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0797-VMLARBSPZULSUMWKZ   PIC X(01).
             10 PI-DRKI0797-VMLBEITRGESWKZ   PIC X(01).
             10 PI-DRKI0797-VMLENDESTEUERLFZ   PIC X(10).
             10 PI-DRKI0797-VMLVLBEITRAGGES   PIC S9(9)V9(2) COMP-3.

      *
       01  PO-DRKI0797.
           05  PO-DRKI0797-RC                 PIC  X(003).
               88 PO-DRKI0797-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0797-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
