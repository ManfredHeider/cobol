      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0337.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0337.CPv  $
      *
      *   Rev 2.3   Aug 06 2015 09:16:32   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 2.2   Jul 16 2015 15:16:06   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 2.1   Mar 08 2000 11:08:40   T13EX22
      *APA00081: Datenmodellanpassung für weitere Produktgeber
      *
      *   Rev 2.0   Feb 27 1999 14:28:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:53:00   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   Apr 24 1997 14:31:20   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0337                                      *
      *    --------------------------------------------------------   *
      *    PDIF-DATEN                                                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    12.06.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!12.06.97! 01 ! NEU                       ! WOLFF    *
      * AFPA0012!26.09.97!    ! ÄNDERUNG DER STELLEN BEI  ! WOLFF    *
      *         !        !    ! BETRÄGEN                  !          *
      * APA00081!23.02.00!    ! ERWEITERUNG UM PGEB-GES   ! MUND     *
      * APA01932!29.06.15!2.01! NEUES FELD HINZU          ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN FÜR PDIF.                   *
      *                                                              *
      ****************************************************************
      *
             30 PDIF-DATEN.
                35 PGEB-GES                         PIC S9(04) BINARY.
                35 PDTA-ID                          PIC S9(015)
                                                        PACKED-DECIMAL.
                35 PDIF-PGV                         PIC S9(004) BINARY.
                35 PDIF-VSTAT                       PIC  X(004).
                35 PDIF-ANZ-BEWEGUNGEN              PIC S9(009) BINARY.
                35 PDIF-GUTSCHRIFTEN                PIC S9(013)V9(2)
                                                        PACKED-DECIMAL.
                35 PDIF-BELASTUNGEN                 PIC S9(013)V9(2)
                                                        PACKED-DECIMAL.
290615          35 PDIF-LAST-UPDATE                 PIC X(026).
290615          35 PDIF-KENNUNG                     PIC X(008).
290615          35 PDIF-UST-BETRAG-GUT              PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
290615          35 PDIF-UST-BETRAG-BEL              PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
      *
      ******* ENDE COPY-MEMBER **************************************

