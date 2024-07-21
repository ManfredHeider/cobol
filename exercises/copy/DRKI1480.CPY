      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1480.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1480.CPv  $
      *
      *   Rev 2.1   Jan 15 2003 15:08:06   t13ex39
      *AKI00303 KoKü
      *
      *   Rev 2.0   Oct 16 2002 08:31:44   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI1475 FUNKTION:VERTRAGSDATEN ERMITTELN    *
      *                                   LIST                       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WHO
      *    DATUM:    15.11.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      * AKI00303!18.09.02! 1.0! NEU FÜR KOKÜ              ! HORNUNG  *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI1475                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION:                                                 *
      *              - ALLE GUTHABENBUCHUNGEN ZU EINER GESELLSCHAFT  *
      *                ERMITTELN                                     *
      *                                                              *
      ****************************************************************
       01  PI-DRKI1480.
           05  PI-DRKI1480-KEY.
             10 PI-DRKI1480-VRBVERTRAGSID               PIC X(13).
             10 PI-DRKI1480-VRBPERSONENROLLEKZ          PIC X(01).
             10 PI-DRKI1480-VRBKOMMBETRKZ               PIC X(01).
      *
       01  PO-DRKI1480.
           05  PO-DRKI1480-RC                           PIC  X(003).
               88 PO-DRKI1480-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1480-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1480-DATEN.
             10 PO-DRKI1480-VRBPERSONENID                PIC X(13).

      *
      ******** Ende Copy-Member **************************************

