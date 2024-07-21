      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   ctpa0010.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\ctpa0010.CPv  $
      *
      *   Rev 2.1   Feb 23 2000 13:56:48   T13EX21
      *APA00081 - Erweiterung um PGEB-GES
      *
      *   Rev 2.0   Feb 27 1999 14:30:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.5   19 Nov 1998 15:29:54   T13EX26
      *apa00033 - Rueckerstellung
      *
      *   Rev 1.4   18 Nov 1998 16:29:18   T13EX26
      *apa00033 - Wiederherstellung
      *
      *   Rev 1.3   13 Nov 1998 09:06:56   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.2   Mar 06 1997 14:12:06   P13PVCS
      *Neuerstellung
      *
      *   Rev 1.1   Oct 31 1996 16:34:56   T13AE19
      *neuerstellung
      *
      *   Rev 1.0   Oct 31 1996 14:26:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CTPA0010                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON TBPA0010                                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SEIBEL                                           *
      *    DATUM:    30.10.1996                                       *
      *****************************************************************
      *    VERSION
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!23.09.98! 02 ! ANPASSUNG VERSION 3.0     ! LÜTTGES  *
      *         !        !    !                           !           *
      * APA00081!09.02.00!2.1 ! ERWEITERUNG UM PGEB-GES   ! STORCH    *
      *         !        !    !                           !           *
      *         !        !    !                           !          *
      ******************************************************************
      * DCLGEN TABLE(T13AE.TBPA0010)                                   *
      *        LIBRARY(T130003.CE010000.SRCDCL(CTPA0010))              *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
BB    *    EXEC SQL DECLARE T13AE.TBPA0010 TABLE
CCC005*    EXEC SQL DECLARE TBPA0010 TABLE
CCC004*    ( PGEB_GES                         SMALLINT NOT NULL,
CCC004*      PROT_ID                          DECIMAL(15, 0) NOT NULL,
CCC004*      KNR_PROT_ART                     SMALLINT NOT NULL,
CCC004*      PROT_ERSTELL_DAT                 DATE NOT NULL,
CCC004*      KNR_OBJEKT_HKF_TAB               SMALLINT NOT NULL,
CCC004*      PROT_PTR_ID                      DECIMAL(15, 0) NOT NULL,
CCC004*      KNR_PROT_PTR_OBJ                 SMALLINT NOT NULL,
CCC004*      KNR_PROT_PTR_AUSPR               SMALLINT NOT NULL,
CCC004*      PROT_LAST_UPDATE                 TIMESTAMP NOT NULL,
CCC004*      PROT_KENNUNG                     CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13AE.TBPA0010                     *
      ******************************************************************
BB    *01  DCLTBPA0010.
BB     01  TBPA0010-GESAMT.
BB         05  DCL-TBPA0010.
090200         10 PGEB-GES           PIC S9(004) BINARY.
               10 PROT-ID            PIC S9(15)V USAGE COMP-3.
               10 KNR-PROT-ART       PIC S9(004) BINARY.
               10 PROT-ERSTELL-DAT   PIC  X(010).
               10 KNR-OBJEKT-HKF-TAB PIC S9(004) BINARY.
               10 PROT-PTR-ID        PIC S9(015) PACKED-DECIMAL.
               10 KNR-PROT-PTR-OBJ   PIC S9(004) BINARY.
               10 KNR-PROT-PTR-AUSPR PIC S9(004) BINARY.
               10 PROT-LAST-UPDATE   PIC  X(026).
               10 PROT-KENNUNG       PIC  X(008).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
