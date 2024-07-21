      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1294.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1294.CPv  $
      *
      *   Rev 2.2   May 09 2006 10:28:02   t13ex39
      *AKI00215: Umstellung aller Fremdverträge aud EURO
      *
      *   Rev 2.1   May 09 2006 10:12:30   t13ex39
      *AKI00215: Umstellung aller Fremdverträge aud EURO
      *
      *   Rev 2.0   05 Jul 2001 17:07:30   p13sp99
      *- Neuerstellung -
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1294                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1294                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  24.07.01/14.47.38
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
      *    DIESES COPY ENTHŽLT DIE EIN- UND AUSGABEPARAMETER
      *    FšR DIE FUNKTION DRKI1294
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1294.
           05  PI-DRKI1294-KEY.


             10 PI-DRKI1294-VMLVERTRAGSID   PIC X(13).

      *
       01  PO-DRKI1294.
           05  PO-DRKI1294-RC                 PIC  X(003).
               88 PO-DRKI1294-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1294-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1294-DATEN.


             10 PO-DRKI1294-VMLARBEITGEBZUSCH   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1294-N-VMLRBTGBZSCH   PIC S9(4) COMP.
             10 PO-DRKI1294-VMLARBEITNEHMSPAR   PIC X(01).
             10 PO-DRKI1294-VMLARBGEBZUSCHWKZ   PIC X(01).
             10 PO-DRKI1294-VMLARBSPARZULDAT   PIC X(10).
             10 PO-DRKI1294-VMLARBSPARZULSUM   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1294-N-VMLRBSPRZLSM   PIC S9(4) COMP.
             10 PO-DRKI1294-VMLARBSPZULSUMWKZ   PIC X(01).
             10 PO-DRKI1294-VMLBEITRGESWKZ   PIC X(01).
             10 PO-DRKI1294-VMLENDESTEUERLFZ   PIC X(10).
             10 PO-DRKI1294-VMLVLBEITRAGGES   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1294-NI-VMLVLBEITRAGGES   PIC S9(4) COMP.


      *
      ******* ENDE COPY-MEMBER **************************************
