      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0163.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/MF/CPY/DRAT0163.CPv  $
      *
      *   Rev 2.5   Jul 04 2002 10:50:32   t13ex21
      *FPA00391 - K-BEITRAG-ABWEICH. auf 5 Cent zurückgesetzt
      *
      *   Rev 2.4   Mar 26 2002 16:28:38   t13ex21
      *APA00185 - Erhöhung K-BEITRAG-ABWEICHUNG auf 20 Cent
      *
      *   Rev 2.3   Dec 18 2001 14:40:58   t13ex33
      *AAT00022 - Euro-Umstellung
      *
      *   Rev 2.2   18 Sep 2001 17:04:14   t13ex33
      *APA00143 - EURO-Umstellung
      *
      *   Rev 2.1   10 Jan 2001 09:35:26   T13EX25
      *FPA00227; Neue Konstante eingefügt
      *
      *   Rev 2.0   Feb 27 1999 11:57:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   13 Nov 1998 09:07:12   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   05 Sep 1997 09:20:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0163                                      *
      *    --------------------------------------------------------   *
      *    KONSTANTEN MIT GEBROCHENEN ZAHLEN FüR DIE ALLGEMEINE       *
      *    UND PROJEKTSPEZIFISCH VERWENDUNG.                          *
      *                                                               *
      *    IN EINIGEN FäLLEN WERDEN KONSTANTEN MIT GEBROCHENEN        *
      *    ZAHLEN BENöTIGT Z.B. ZULäSSIGE ABWEICHUNG. IN ALLEN NEUEN  *
      *    PROGRAMME IST DIE OPTION 'DECIMAL POINT IS COMMA' AKTIV.   *
      *    EINIGEN ALTPROGRAMMEN WIRD DER PUNKT VERWENDET.            *
      *    UM PROBLEME ZU VERMEIDEN WERDEN DIE KONSTANTEN             *
      *    - IM DRAT0163 MIT KOMMA UND                                *
      *    - IM DRAT0164 MIT PUNKT DEFINIERT.                         *
      *    NEUE KONSTANTEN SIND IN BEIDEN COPIES EINZUTRAGEN.         *
      *    --------------------------------------------------------   *
      *    ERSTELLT: AUERBACH                                         *
      *    DATUM:    20.10.1997                                       *
      *****************************************************************
      *    VERSION   1    08.06.95 16:19:49
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME     *
      * --------------------------------------------------------------*
      * AFPA0012!20.10.97!01.01! NEU                       ! AUERBACH *
      *         !        !     !                           !          *
      * AFPA0012!12.01.98!01.02! NEU: K-BEITRAG-ABWEICHUNG ! WOLFF    *
      * AFPA0012!03.11.98!01.03! GEÄNDERT: DAISY-ABWEICHUNG! MUND     *
      *         !        !     !                           !          *
      * FPA00227!10.01.01!01.04! NEUE KONSTANTE FÜR EVS    ! JÄGER    *
EURO*** APA00143!07.06.01!02.03! EURO-UMSTELLUNG           ! ANIKEEV  *
      * APA00185!26.03.02!02.04! K-BEITRAG-ABWEICHUNG AUF  ! STORCH   *
      *         !        !     ! 20 CENT ERHÖHT            !          *
      * FPA00391!04.07.02!02.05! K-BEITRAG-ABWEICHUNG AUF  ! STORCH   *
      *         !        !     ! 5 CENT ZURÜCKGESETZT      !          *
      *         !        !     !                           !          *
      *         !        !     !                           !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************

      ***  KONSTANTEN FÜR GEBROCHENE ZAHLEN
       01  FILLER.

      ***  DEFINIERTE GRUNDABWEICHUNG IN DAISY
031198*    05  K-DAISY-ABWEICHUNG         PIC  9(001)V9(2) VALUE 0,01.
 EURO-*    05  K-DAISY-ABWEICHUNG         PIC  9(001)V9(2) VALUE 0,05.
 EURO-*    05  K-BEITRAG-ABWEICHUNG       PIC  9(001)V9(2) VALUE 0,10.
 EURO-*    05  K-BEITRAG-ABWEICHUNG-EVS   PIC  9(001)V9(2) VALUE 5,00.
 EURO+     05  K-DAISY-ABWEICHUNG         PIC  9(001)V9(2) VALUE 0,03.
 EURO+*    05  K-BEITRAG-ABWEICHUNG       PIC  9(001)V9(2) VALUE 0,05.
260302*    05  K-BEITRAG-ABWEICHUNG       PIC  9(001)V9(2) VALUE 0,20.
040702     05  K-BEITRAG-ABWEICHUNG       PIC  9(001)V9(2) VALUE 0,05.
 EURO+     05  K-BEITRAG-ABWEICHUNG-EVS   PIC  9(001)V9(2) VALUE 2,50.
      *
      ******** ENDE COPY-MEMBER STANDARD-KONSTANTEN (DRAT0163) ********
