      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1269.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1269.CPv  $
      *
      *   Rev 2.2   May 09 2006 10:27:20   t13ex39
      *AKI00215: Umstellung aller Fremdverträge aud EURO
      *
      *   Rev 2.1   May 09 2006 10:11:50   t13ex39
      *AKI00215: Umstellung aller Fremdverträge aud EURO
      *
      *   Rev 2.0   05 Jul 2001 17:07:08   p13sp99
      *- Neuerstellung -
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1269                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1269                    *
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
      *    FšR DIE FUNKTION DRKI1269
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1269.
           05  PI-DRKI1269-KEY.


             10 PI-DRKI1269-VMLVERTRAGSID   PIC X(13).


           05  PI-DRKI1269-DATEN.


             10 PI-DRKI1269-VMLARBEITGEBZUSCH   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1269-N-VMLRBTGBZSCH   PIC S9(4) COMP.
             10 PI-DRKI1269-VMLARBEITNEHMSPAR   PIC X(01).
             10 PI-DRKI1269-VMLARBGEBZUSCHWKZ   PIC X(01).
             10 PI-DRKI1269-VMLARBSPARZULDAT   PIC X(10).
             10 PI-DRKI1269-VMLARBSPARZULSUM   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1269-N-VMLRBSPRZLSM   PIC S9(4) COMP.
             10 PI-DRKI1269-VMLARBSPZULSUMWKZ   PIC X(01).
             10 PI-DRKI1269-VMLBEITRGESWKZ   PIC X(01).
             10 PI-DRKI1269-VMLENDESTEUERLFZ   PIC X(10).
             10 PI-DRKI1269-VMLVLBEITRAGGES   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1269-NI-VMLVLBEITRAGGES   PIC S9(4) COMP.


      *
       01  PO-DRKI1269.
           05  PO-DRKI1269-RC                 PIC  X(003).
               88 PO-DRKI1269-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1269-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
