      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0704.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI0704.CPv  $
      *
      *   Rev 2.2   Jun 08 2018 12:29:58   t13ae31
      *AKI01567: G10 Umstellung auf KSDS in DB2 Pgm
      *
      *   Rev 2.1   Jun 07 2013 09:08:26   t13ex73
      *AKI01204: SEPA - DB-Online - Kfz
      *
      *   Rev 2.0   Mar 05 1999 11:36:38   T13EX41
      *
      *
      *   Rev 1.0   06 Oct 1998 15:30:56   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
 
      *****************************************************************
      *                                                               *
      *    COPY-MEMBER ZU DEN ERMITTLUNGSPROGRAMMEN                   *
      *     - Eingabedatei AMV-Kundendaten mit Vorsatz                *
      *                                                               *
      *    SATZLÄNGE   300                                            *
140113*    SATZLÄNGE   511                                            *
060513*    SATZLÄNGE   500                                            *
      *                                                               *
      *---------------------------------------------------------------*
      *    LOGBUCH                                                    *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS.*
      *AUFTRAG !                                      !         !     *
      *--------!--------------------------------------!---------!-----*
      *21.10.98!NEUERSTELLUNG                         !B.GOLÜCKE! 01  *
      *--------!--------------------------------------!---------!-----*
140113*AKI01204!14.01.13! neue Felder für SEPA        !WMI      !     *
      *--------!--------------------------------------!---------!-----*
      *AKI01204!06.05.13! FILLER X11 auskommentieren  !DDY      !     *
      *        !        ! Satzlänge anpassen          !         !     *
      *--------!--------------------------------------!---------!-----*
050218*AKI01567!05.02.18! Einbau SPARTE (KFZ/SUH)     !HHU      !     *
      *****************************************************************
 
       01  DRKI0704.
 
           05  AMV-KUN-VORSATZ.
      *
               10  AMV-KUN-V-SA                  PIC X(001).
      *            SATZART = 'A'
               10  AMV-KUN-V-ABSENDER            PIC X(003).
      *            ABSENDER = 'AMV'
               10  AMV-KUN-V-DATUM               PIC 9(008).
      *            ERFASSUNGS-/ÄNDERUNGSDATUM = 'JJJJMMTT'
               10  AMV-KUN-V-EMPFAENGER          PIC X(004).
      *            EMPFAENGER = 'DVAG'
               10  AMV-KUN-V-TEXT                PIC X(016).
      *            INHALTSANGABE = 'KUNDEN BESTAND'
               10  AMV-KUN-V-KZ                  PIC X(004).
      *            INHALTSANGABE = 'INIT' für Komplettbestand
      *                            'AEND' für Änderungsbestand
140113*        10  FILLER                        PIC X(264).
140113*        10  FILLER                        PIC X(475).
050613         10  FILLER                        PIC X(464).
      *            RESERVE
 
           05  AMV-KUN-SATZ REDEFINES AMV-KUN-VORSATZ.
 
               10  AMV-KUN-AEND-KZ               PIC 9(002).
               10  AMV-KUN-NACH-NAME             PIC X(040).
               10  AMV-KUN-VOR-NAME              PIC X(030).
               10  AMV-KUN-GEB-DATUM.
                   15  AMV-KUN-GEB-JH            PIC 9(002).
                   15  AMV-KUN-GEB-DAT.
                       20  AMV-KUN-GEB-JJ        PIC 9(002).
                       20  AMV-KUN-GEB-MM        PIC 9(002).
                       20  AMV-KUN-GEB-TT        PIC 9(002).
               10  AMV-KUN-ANREDE-KZ             PIC 9(002).
               10  AMV-KUN-GESCHLECHT            PIC 9(001).
               10  AMV-KUN-STRASSE               PIC X(045).
               10  AMV-KUN-POSTFACH              PIC X(045).
               10  AMV-KUN-POST-LZ               PIC X(009).
               10  AMV-KUN-WOHNORT               PIC X(040).
               10  AMV-KUN-AUSL-KZ               PIC X(003).
               10  AMV-KUN-TELEFON-NR            PIC X(015).
               10  AMV-KUN-KOMM-KZ               PIC 9(002).
140113*        10  AMV-KUN-VERT-NR               PIC 9(015).
140113*        10  AMV-KUN-ROLLE-KZ              PIC 9(002).
140113         10  AMV-KUN-KEY.
140113             15  AMV-KUN-VERT-NR           PIC 9(015).
140113             15  AMV-KUN-ROLLE-KZ          PIC 9(002).
140113*            01=VN, 02=BZ, 03=VN+BZ
               10  AMV-KUN-NAME-AEND-KZ          PIC X(001).
               10  AMV-KUN-ADRESS-AEND-KZ        PIC X(001).
               10  AMV-KUN-SONST-AEND-KZ         PIC X(001).
140113*        10  AMV-KUN-FILLER                PIC X(001).
140113         10  AMV-KUN-NAT-JUR-PERSON        PIC X(001).
               10  AMV-KUN-TIMESTAMP             PIC X(026).
      *            JJJJ-MM-TT-hh.mm.ss.abcdef
      *            abcdef = Millisekunden (wird mit 0 geliefert)
060513*        10  FILLER                        PIC X(011).
140113         10  AMV-KUN-PARTNER-HOST-ID       PIC X(015).
140113         10  AMV-KUN-IBAN                  PIC X(034).
140113         10  AMV-KUN-BIC                   PIC X(011).
140113         10  FILLER                        PIC X(148).
050218**** SPARTE KFZ / SUH da DB2 KSDS für beide in einer Tabelle
050218         10  AMV-KUN-SPARTE                PIC X(003).
 
           05  AMV-KUN-NACHSATZ REDEFINES AMV-KUN-VORSATZ.
      *
               10  AMV-KUN-N-SA                  PIC X(001).
      *            SATZART = 'E'
               10  AMV-KUN-N-SATZANZAHL          PIC 9(009).
      *            SATZANZAHL OHNE VOR- UND NACHSATZ
140113*        10  FILLER                        PIC X(290).
140113*        10  FILLER                        PIC X(501).
060513         10  FILLER                        PIC X(490).
      *            RESERVE
 
      ******************************************************************
      *          E n d e    Copy-Member DRKI0704                       *
      ******************************************************************

