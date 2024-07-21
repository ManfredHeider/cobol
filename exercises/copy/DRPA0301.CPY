      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0301.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0301.CPv  $
      *
      *   Rev 2.3   Jun 19 2012 09:22:46   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   May 21 2012 08:47:44   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.1   Dec 10 2007 08:52:40   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.0   Feb 27 1999 14:28:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:50:10   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   May 26 1997 15:21:42   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0301                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<NPM ERSTELLEN AUS PM>>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    09.06.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!09.06.97! 01 ! NEU                       ! WOLFF    *
      * AFPA0012!        !    ! CRPA0018                  !          *
      *         !18.09.98!    !   - MELDUNGSFEHLER RAUS-  ! WOLFF    *
      *         !        !    !     GENOMMEN              !          *
      * APA00622!10.08.07!2.01! NEU: PI-DRPA0301-KNR-VGES ! STORCH   *
      *         !        !    !      PI-DRPA0301-ABSENDER !          *
      * APA01101!29.02.12!2.02! VGES DURCH MANDANT ERSETZT! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0301                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0301.
          05 PI-DRPA0301-DATEN.
             10 PI-DRPA0301-PGEB-GES                PIC S9(004) BINARY.
             10 PI-DRPA0301-ERSTELL-DAT             PIC  X(010).
             10 PI-DRPA0301-PMPG-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA0301-PMPG-VERSION            PIC S9(004) BINARY.
             10 PI-DRPA0301-PMPG-LFD-NR-VNR         PIC S9(009) BINARY.
             10 PI-DRPA0301-PDTA-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA0301-KNR-MANDANT             PIC S9(004) BINARY.
             10 PI-DRPA0301-ABSENDER                PIC  X(030).
             10 PI-DRPA0301-PMPG-INHALT             PIC X(3900).
      *
       01 PO-DRPA0301.
          05 PO-DRPA0301-RC                         PIC  X(003).
             88 PO-DRPA0301-RC-OK                         VALUE 'OK '.
      *
      ******* ENDE COPY-MEMBER **************************************
