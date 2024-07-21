      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.04.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2855.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2855.CPv  $
      *
      *   Rev 2.3   Mar 27 2017 09:30:46   t13ae31
      *FKI01544: FVD Teil 1 alten Code entfernen COPY
      *
      *   Rev 2.2   May 23 2014 11:20:34   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 K
      *ündigung PGM Cpy
      *
      *   Rev 2.1   May 13 2014 16:25:48   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 K
      *ündigung PGM Cpy
      *
      *   Rev 2.0   Apr 04 2014 10:15:32   t13ex15
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 2 8 5 5    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER G40 OIF Kuendigung DTA                       *
      *
      *    SATZLÄNGE   100 Bytes                                       *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *04.04.14!NEUERSTELLUNG                          !SWZ      ! 01  *
      *--------!---------------------------------------!---------!-----*
      *24.03.17! FVD entfernt                          !HHU      !     *
      ******************************************************************
       01  DRKI2855.
           10  'XX'-VORSATZ.
      *
               20  'XX'-V-SA            PIC X(001).
      *            SATZART = 'A'
               20  'XX'-V-ABSENDER      PIC X(008).
      *            ABSENDER = ''XX',HOF'
               20  'XX'-V-DATUM         PIC 9(008).
      *            BUCHUNGSDATUM = 'JJJJMMTT'
               20  'XX'-V-EMPFAENGER    PIC X(004).
      *            EMPFAENGER = 'DVAG'
               20  'XX'-V-TEXT          PIC X(010).
      *            INHALTSANGABE:
      *            Tägliche Lieferung DVAG              'OIFDVAKP'
      *            Tägliche Lieferung ALLF              'OIFALFKP'
 
               20  'XX'-V-INHALT        PIC X(001).
      *            DATENSÄTZE ÜBERTRAGEN: JA = SPACE   NEIN = 'K'
 
               20  'XX'-V-WKZ           PIC X(003).
      *            Währungs-Kennz. =  'EUR'
               20  FILLER              PIC X(065).
      *            RESERVE
 
           10  'XX'-SATZ     REDEFINES      'XX'-VORSATZ.
      *
               20  'XX'-SA                  PIC  X(001).
      *            SATZART        = 'C'
               20  'XX'-GES                 PIC  9(002).
      *            GESELLSCHAFT   = '40'
               20  'XX'-KEY.
                   25  'XX'-VNR                 PIC  9(015).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
                   25 FILLER REDEFINES 'XX'-VNR.
                      30  'XX'-VNR-1-13         PIC  9(013).
                      30  'XX'-UNTER-NR         PIC  9(002).
      *            UNTERDEPOTNUMMER)
 
                   25  'XX'-ISIN                PIC  X(12).
      *            ISIN
 
                   25  'XX'-KTO-ART             PIC  X(02).
      *            Kontoart
 
                   25  'XX'-RUECKZAHL-TERMIN    PIC  9(08).
      *            voraussichtlicher Rückzahlungstermin
 
      *        *****************************************************
      *        *   OIF/ANSFUG
      *        *****************************************************
               20  'XX'-ANZ-ANTEILE         PIC  9(13)V9999.
      *            Anzahl Anteile
      *        *****************************************************
               20  FILLER                  PIC  X(43).
      *            RESERVE
 
 
           10  'XX'-NACHSATZ    REDEFINES     'XX'-VORSATZ.
      *
               20  'XX'-N-SA            PIC X(001).
      *            SATZART             'E'
               20  'XX'-N-ANZ           PIC 9(009).
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
      *        20  FILLER              PIC X(120).
               20  FILLER              PIC X(090).
      *            RESERVE
 

