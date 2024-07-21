      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0125.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0125.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 15:13:38   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0125                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0125                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHÄFER                                          *
      *    DATUM:    22.01.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0002!22.01.97! 01 ! NEU                       ! SCHÄFER  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0125: LESEN MS
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRVB0125.
           05  PI-DRVB0125-DATEN.
               10 PI-DRVB0125-MS-PROV-STF      PIC S9(003)V99 COMP-3.
               10 PI-DRVB0125-PEWA-STRUK-DAT   PIC  X(010).
      *
       01  PO-DRVB0125.
           05  PO-DRVB0125-RC                  PIC  X(003).
               88 PO-DRVB0125-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0125-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRVB0125-DATEN.
               10 PO-DRVB0125-MS-NR            PIC  9(004) BINARY.
               10 PO-DRVB0125-MS-BEZ-KURZ      PIC  X(004).
      *
      ******* ENDE COPY-MEMBER DRVB0125 *****************************
