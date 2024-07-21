      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0475.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRVB0475.CPv  $
      *
      *   Rev 2.2   17 May 2000 12:13:14   T13AE26
      *ASL00046 Förderbonus Auszahlung
      *
      *   Rev 2.1   10 May 2000 14:03:46   T13AE26
      *ASL00046 Förderbonus Auszahlung
      *
      *   Rev 2.0   Apr 03 2000 14:11:04   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0475                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0475                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    04.05.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00046!04.05.00!    ! NEU                       ! Walb     *
      * ASL00046!15.05.00! 2.2! FBAZ-ANSPR-VOM entfernt   ! Walb     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0475: gesamtes Tupel aus FBAZ lesen
      *
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRVB0475.
           05  PI-DRVB0475-DATEN.
               10 PI-DRVB0475-ST-NR             PIC S9(009) COMP.
               10 PI-DRVB0475-FBAZ-UNTST-NR     PIC S9(009) COMP.
               10 PI-DRVB0475-KNR-ANSPR-FBONUS  PIC S9(004) COMP.
150500*        10 PI-DRVB0475-FBAZ-ANSPR-VOM    PIC X(010).
      *
       01  PO-DRVB0475.
           05  PO-DRVB0475-RC                   PIC  X(003).
               88 PO-DRVB0475-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0475-RC-NOT-FOUND                 VALUE 'NFD'.

           05  PO-DRVB0475-DATEN.
               10 PO-DRVB0475-FBAZ-TUPEL-GESAMT PIC  X(084).
      *
      ******* ENDE COPY-MEMBER **************************************
