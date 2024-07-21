      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0561.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRKI0561.CPv  $
      *
      *   Rev 2.2   Dec 01 1999 16:10:04   T13AE38
      *AKI00057
      *
      *   Rev 2.1   Nov 29 1999 16:53:22   T13AE38
      *AKI00057
      *
      *   Rev 2.0   Feb 27 1999 12:57:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 Feb 1999 10:18:04   T13AE38
      *AKI00014 - copystrecke fuer Fehlerprotokolle Ermittlungpgm
      *
      *   Rev 1.0   07 Aug 1998 14:48:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 0 5 6 1    ########                    *
      *              #####################                             *
      *                                                                *
      *     Copy-Strecke zur Aufbereitung von Fehlermeldungen in  das  *
      *     allgemeine Fehlerprotokoll (nach Erstellung z.Zt. nur für  *
      *     das KI-Projekt                                             *
      *                                                                *
      *     AUF VIELFACHEN WUNSCH EINES PROGRAMMIERERS WIRD DAS FELD   *
      *     FEHLER-FILLER WIEDER AUFGENOMMEN, DADURCH äNDERT SICH DIE  *
      *     SATZLäNGE AUF 150                                          *
      *                                                                *
      *                                                                *
      *    SATZLÄNGE   150                                             *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *05.06.98!Neu-Erstellung                         !KH Müller! 01  *
      *KI-NEU  !                                       !         !     *
      *21.09.98!Erweiterung Teiltyp- und Teilschicht-  !S.Jost   !     *
      *        !nummer hinzugefügt                     !         !     *
      *14.01.99!ERSETZEN FEHLER-UHRZEIT DURCH FEHLER-  !PHILIPP  !     *
      *        !PGM-BEZEICHNUNG                        !         !     *
      *18.01.99!FEHLER-FILLER WIEDER AUFGENOMMEN       !PHILIPP  !     *
      *        !                                       !         !     *
      *01.11.99!ÄNDERUNG BEI AUSGABE-FELD FEHLER-DATUM !S.JOST   ! 1.4 *
      *        !IN FEHLERDATUM BZW FEHLERHAFTES FELD   !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************

       01  DRKI0561.

      *                                FEHLER-DATUM IM DB2-FORMAT
      *                                (JHJJ-MM-TT) EINSTELLEN:
           05  FEHLER-DATUM            PIC  X(010).

011199*                                AUSGABE FEHLERHAFTES FELD
011199*                                EINGEFÜGT
011199     05  FEHLER-FEHLERFELD REDEFINES FEHLER-DATUM PIC X(010).

      *                                Fehler-Uhrzeit im DB2-Format
      *                                (HH:MM:SS) einstellen:
140199*    05  FEHLER-UHRZEIT             PIC  X(008).
140199     05  FEHLER-PGM-BEZEICHNUNG     PIC  X(024).

      *                                Programmname des den Fehler
      *                                erzeugenden Programms:

           05  FEHLER-PGMNAME             PIC  X(008).

      *                                Gesellschafts-ID:
           05  FEHLER-GESELLSCHAFT        PIC  9(002).

      *                                Vertrags-Nummer:
           05  FEHLER-VERTRAGSNR          PIC  9(015).

      *                                Interne Vertrags-Nummer:
           05  FEHLER-VERTRAGSNR-INTERN   PIC  9(007).

      *                                Fehlernummer und Fehlertext
      *                                sind gemäß den Eintragungen
      *                                in Copy-Strecke    DRKI0132
      *                                einzutragen:
           05  FEHLER-FEHLERNUMMER        PIC  X(004).

140199*    05  FEHLER-FILLER1             PIC  X(001).

      *                                Fehlertext bitte ohne die ersten
      *                                   5 Stellen (= Fehlernummer)
      *                                eintragen:
           05  FEHLER-FEHLERTEXT          PIC  X(045).
      *                                Teitltyp-Nummer:
           05  FEHLER-TEILTYP             PIC  9(002).
      *                                Teilschicht-Nummer:
           05  FEHLER-TEILSCHICHT         PIC  9(003).
      *
140199*    05  FEHLER-FILLER              PIC  X(015).
180199     05  FEHLER-ZUSATZINFO          PIC  X(015).
180199     05  FILLER                     PIC  X(015).

      ******************************************************************
      *          E n d e    Copy-Buch   DRKI0561                       *
      ******************************************************************
