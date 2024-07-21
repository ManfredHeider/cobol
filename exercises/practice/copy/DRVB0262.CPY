      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drvb0262.cpy  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\drvb0262.cpv  $
      *
      *   Rev 2.2   Jan 11 2000 10:49:06   T13AE34
      *AAN00237 - MIS Antragsbereich
      *
      *   Rev 2.1   Dec 15 1999 16:13:12   T13AE42
      *AAS00177 - neue Funktion eingebaut
      *
      *   Rev 2.0   Feb 27 1999 15:58:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   23 Jul 1997 12:23:32   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0262                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0262                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT,B.                                      *
      *    DATUM:    15.12.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00177!15.12.99! 01 ! NEU                       !B.SCHMIDT *
      */AAN00237!        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0262
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0262.
           05  PI-DRVB0262-DIR-KEY.
               10 PI-DRVB0262-DIR-NR         PIC S9(004) BINARY.
      *
       01  PO-DRVB0262.
           05  PO-DRVB0262-RC                 PIC  X(003).
               88 PO-DRVB0262-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0262-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRVB0262-KNR-BEZ            PIC   X(010).
      *
      ******* ENDE COPY-MEMBER **************************************
