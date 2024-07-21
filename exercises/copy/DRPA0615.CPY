      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0615.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0615.CPv  $
      *
      *   Rev 2.0   May 27 1999 11:31:14   T01JM2S
      *NT-Umstellung
      *
      *   Rev 1.0   23 Jul 1997 12:35:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0615                                      *
      *    --------------------------------------------------------   *
      *    BESTANDSDATEN-TABELLE                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    11.11.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!11.11.97! 01 ! NEU                       ! PRIGGEN  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN FÜR DIE TABELLE MIT         *
      *    BESTANDSMELDUNGEN                                         *
      *                                                              *
      *    BEI ÄNDERUNG DER OCCURS-ANZAHL DIE COPY-STRECKE           *
      *                                                              *
      *       - DRPA0639                                             *
      *                                                              *
      *    ANPASSEN.                                                 *
      *                                                              *
      ****************************************************************
      *
             12 MELD-TABELLE.
                13 MAX-USED-MELD                    PIC S9(004) BINARY.
                13 FILLER                           OCCURS 10
                                                    INDEXED BY IX-MELD.
                   14 MELD-PMBE.
                      COPY DRPA0411.
                   14 MELD-PMPR.
                      COPY DRPA0432.
                   14 MELD-PMPT.
                      COPY DRPA0421.
      *
      ******* ENDE COPY-MEMBER **************************************
