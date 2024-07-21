      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.07.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2767.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2767.CPv  $
      *
      *   Rev 2.3   Nov 12 2013 12:03:48   t13ex52
      *FKI01434: Ersetzen der ObjektId durch die VertragsId in KFZ-
      *Tabellen
      *
      *   Rev 2.2   Jul 05 2013 10:30:24   t13ex52
      *AKI01218: G10 KFZ Verarbeitg. zukünftiger Beiträge bei flexi
      *bler Hauptfälligkeit
      *
      *   Rev 2.1   Jul 04 2013 14:59:28   t13ex52
      *AKI01218: G10 KFZ Verarbeitg. zukünftiger Beiträge bei flexi
      *bler Hauptfälligkeit
      *
      *   Rev 2.0   Jul 02 2013 09:42:00   t13ex52
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER ZU DEN ERMITTLUNGSPROGRAMMEN                    *
      *     - Eingabedatei AMV-Vertragsdaten für KFZ mit Vorsatz       *
      *                                                                *
      *    SATZLÄNGE   116                                             *
      *--------------------------------------------------------------- *
      *LOGBUCH                                                         *
      *==========
      *        !        ! AENDERUNG                      !NAME         *
      *AUFTRAG !        !                                !             *
      *--------!--------!--------------------------------!-------------*
      *AKI01218!02.07.13! NEU                            !H.Schlenther *
      *   UE001!        !                                !             *
      *--------!--------!--------------------------------!-------------*
211013*FKI01434!21.10.13! Ersetzen der ObjektId durch    !H.Schlenther *
      *   UE001!        ! die VertragsID in KFZ-Tabellen !             *
      *--------!--------!--------------------------------!-------------*
      *        !        !                                !
      *--------!--------!--------------------------------!-------------*
      *        !        !                                !             *
      *--------!--------!--------------------------------!-------------*
      *        !        !                                !             *
      ******************************************************************

       01  DRKI2767.

           05 AMV-KFZ-DATEN.
211013*        10  AMV-KFZ-VERSOBJEKT-ID         PIC X(13).
    >>         10  AMV-KFZ-VERTRAGS-ID           PIC X(13).
      *
    <<*        10  AMV-KFZ-VERSOBJEKT-ID-HEX     PIC X(26).
211013         10  AMV-KFZ-VERTRAGS-ID-HEX       PIC X(26).
      *
               10  AMV-KFZ-BEITRAG-ANPASS-AM     PIC X(10).
      *            in der Form DD.MM.JJJJ
               10  FILLER                        PIC X(01).
      *
               10  AMV-KFZ-BERECHNUNG-AM         PIC X(10).
      *            in der Form DD.MM.JJJJ
               10  FILLER                        PIC X(01).
      *
               10  AMV-KFZ-VERVERTRAGS-ID           PIC X(13).
      *
               10  AMV-KFZ-VERVERTRAGS-ID-HEX       PIC X(26).
      *
               10  AMV-KFZ-VERTRAGSNR            PIC X(15).
      *
               10  FILLER                        PIC X(01).
      *
      ******************************************************************
      *          E n d e    Copy-Member DRKI2767                       *
      ******************************************************************
