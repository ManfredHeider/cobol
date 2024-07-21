      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1279.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1279.CPv  $
      *
      *   Rev 2.1   May 03 2006 09:46:32   t13ex39
      *AKI00215: Umstellung EURO  KFZUNFALL
      *
      *   Rev 2.0   05 Jul 2001 17:07:16   p13sp99
      *- Neuerstellung -
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1279                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1279                    *
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
      *    FšR DIE FUNKTION DRKI1279
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1279.
           05  PI-DRKI1279-KEY.


             10 PI-DRKI1279-KFUVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1279-KFUVERTRAGSID   PIC X(13).

      *
       01  PO-DRKI1279.
           05  PO-DRKI1279-RC                 PIC  X(003).
               88 PO-DRKI1279-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1279-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1279-DATEN.


             10 PO-DRKI1279-KFUBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1279-NI-KFUBEITRAG   PIC S9(4) COMP.
             10 PO-DRKI1279-KFUBEITRAGWKZ   PIC X(01).
             10 PO-DRKI1279-KFUINVALIDSUMME   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1279-NI-KFUINVALIDSUMME   PIC S9(4) COMP.
             10 PO-DRKI1279-KFUINVSUMMEWKZ   PIC X(01).
             10 PO-DRKI1279-KFUPROGRESSINVPROZ   PIC S9(3)V9(2) COMP-3.
             10 PO-DRKI1279-N-KFPRGRSSNVPRZ   PIC S9(4) COMP.
             10 PO-DRKI1279-KFUTODSUMME   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1279-NI-KFUTODSUMME   PIC S9(4) COMP.
             10 PO-DRKI1279-KFUTODSUMMEWKZ   PIC X(01).
             10 PO-DRKI1279-KFUUNFALLART   PIC X(01).


      *
      ******* ENDE COPY-MEMBER **************************************
