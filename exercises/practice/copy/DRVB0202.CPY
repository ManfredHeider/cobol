      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0202.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0202.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   16 Dec 1997 09:50:22   T13AE35
      *AVB00400 - Neuerstellung, Fkt. zu ZWVB0006
      *
      *   Rev 1.0   23 Jul 1997 12:14:56   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0202                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0202                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: HERING                                          *
      *    DATUM:    08.12.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00400!08.12.97! 01 ! NEU                       ! J.HERING *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0202
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0202.
           05  PI-DRVB0202-DIR-KEY.
               10 PI-DRVB0202-DIR-NR         PIC S9(004) BINARY.
      *
       01  PO-DRVB0202.
           05  PO-DRVB0202-RC                 PIC  X(003).
               88 PO-DRVB0202-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0202-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRVB0202-KTY-REGION         PIC  S9(004) BINARY.
           05  PO-DRVB0202-KNR-DIR-TYP        PIC  S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
