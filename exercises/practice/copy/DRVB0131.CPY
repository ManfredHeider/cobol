      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0131.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0131.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 15:14:26   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0131                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0131                    *
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
      *    FÜR DIE FUNKTION DRVB0131: LESEN EDED
      *        -   PRüFEN, OB BETREUUNGSVERHÄLTNIS JE BESTANDEN HAT
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRVB0131.
           05  PI-DRVB0131-DATEN.
               10 PI-DRVB0131-EDED-BETR-NR     PIC S9(009) COMP.
               10 PI-DRVB0131-EDED-UNTST-NR    PIC S9(009) COMP.
      *
       01  PO-DRVB0131.
           05  PO-DRVB0131-RC                  PIC  X(003).
               88 PO-DRVB0131-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0131-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
