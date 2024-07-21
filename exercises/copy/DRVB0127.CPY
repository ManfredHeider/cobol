      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0127.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0127.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 15:14:04   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0127                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0127                    *
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
      *    FÜR DIE FUNKTION DRVB0127:
      *    PRüFEN BETREUUNGSVERHäLTNISS AUF STRUKTURELLE VERTRäGLICHKEIT
      *
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRVB0127.
           05  PI-DRVB0127-DATEN.
               10 PI-DRVB0127-BETR-TYP            PIC S9(004) COMP.
               10 PI-DRVB0127-BETR-MS-BEZ-KURZ    PIC  X(004).
               10 PI-DRVB0127-UNTST-TYP           PIC S9(004) COMP.
               10 PI-DRVB0127-UNTST-MS-BEZ-KURZ   PIC  X(004).
               10 PI-DRVB0127-PEWA-STRUK-DAT      PIC  X(010).
      *
       01  PO-DRVB0127.
           05  PO-DRVB0127-RC                     PIC  X(003).
               88 PO-DRVB0127-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0127-RC-NOK                       VALUE 'NOK'.
      *
      ******* ENDE COPY-MEMBER DRVB0127 *****************************
