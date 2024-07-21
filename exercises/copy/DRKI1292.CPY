      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1292.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1292.CPv  $
      *
      *   Rev 2.2   Apr 29 2009 14:58:14   t13ex14
      *AKI00818: Tabelle VERTRAGRABATT OZP-Objekte
      *
      *   Rev 2.0   05 Jul 2001 17:07:28   p13sp99
      *- Neuerstellung -
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1292                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1292                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  22.04.2009/15:54:27
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
      *    FÜR DIE FUNKTION DRKI1292
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1292.
           05  PI-DRKI1292-KEY.


             10 PI-DRKI1292-VRALFDNR   PIC S9(4) COMP.
             10 PI-DRKI1292-VRAVERTRAGSID   PIC X(13).
             
      *
       01  PO-DRKI1292.
           05  PO-DRKI1292-RC                 PIC  X(003).
               88 PO-DRKI1292-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1292-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1292-DATEN.


             10 PO-DRKI1292-VRARABEINHEIT   PIC S9(4) COMP.
             10 PO-DRKI1292-NI-VRARABEINHEIT   PIC S9(4) COMP.
             10 PO-DRKI1292-VRARABTEXT   PIC X(26).
             10 PO-DRKI1292-VRARABWERT   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1292-NI-VRARABWERT   PIC S9(4) COMP.
             10 PO-DRKI1292-VRARABWERWKZ   PIC X(01).
             

      *
      ******* ENDE COPY-MEMBER **************************************
