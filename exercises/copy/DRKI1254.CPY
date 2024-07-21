      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1254.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1254.CPv  $
      *
      *   Rev 2.1   May 03 2006 09:46:18   t13ex39
      *AKI00215: Umstellung EURO  KFZUNFALL
      *
      *   Rev 2.0   05 Jul 2001 17:06:52   p13sp99
      *- Neuerstellung -
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1254                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1254                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  24.07.01/14.44.20
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
      *    FšR DIE FUNKTION DRKI1254
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1254.
           05  PI-DRKI1254-KEY.


             10 PI-DRKI1254-KFUVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1254-KFUVERTRAGSID   PIC X(13).


           05  PI-DRKI1254-DATEN.


             10 PI-DRKI1254-KFUBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1254-NI-KFUBEITRAG   PIC S9(4) COMP.
             10 PI-DRKI1254-KFUBEITRAGWKZ   PIC X(01).
             10 PI-DRKI1254-KFUINVALIDSUMME   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1254-NI-KFUINVALIDSUMME   PIC S9(4) COMP.
             10 PI-DRKI1254-KFUINVSUMMEWKZ   PIC X(01).
             10 PI-DRKI1254-KFUPROGRESSINVPROZ   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI1254-N-KFPRGRSSNVPRZ   PIC S9(4) COMP.
             10 PI-DRKI1254-KFUTODSUMME   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1254-NI-KFUTODSUMME   PIC S9(4) COMP.
             10 PI-DRKI1254-KFUTODSUMMEWKZ   PIC X(01).
             10 PI-DRKI1254-KFUUNFALLART   PIC X(01).


      *
       01  PO-DRKI1254.
           05  PO-DRKI1254-RC                 PIC  X(003).
               88 PO-DRKI1254-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1254-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
