      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0135.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0135.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 15:14:44   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0135                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0135                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHÄFER                                          *
      *    DATUM:    25.01.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0002!25.01.97! 01 ! NEU                       ! SCHÄFER  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0135: LESEN BVH
      *        -   PRüFEN, OB BETREUUNGSVERHÄLTNIS JE BESTANDEN HAT
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRVB0135.
           05  PI-DRVB0135-DATEN.
               10 PI-DRVB0135-BVH-BETR-NR      PIC S9(009) COMP.
               10 PI-DRVB0135-BVH-UNTST-NR     PIC S9(009) COMP.
      *
       01  PO-DRVB0135.
           05  PO-DRVB0135-RC                  PIC  X(003).
               88 PO-DRVB0135-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0135-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
