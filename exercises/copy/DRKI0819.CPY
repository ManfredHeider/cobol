      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0819.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/MF/CPY/DRKI0819.CPv  $
      *
      *   Rev 2.6   Apr 02 2003 13:31:42   t13ae31
      *AKI00333 neue Felder ZINSPLUS BWZVJ ZINSLFDJAHR
      *
      *   Rev 2.5   Jan 25 2002 08:53:22   t13ex49
      *AKI00258 - Dezimalstellen
      *
      *   Rev 2.4   20 Dec 2000 14:01:06   T13EX49
      *AKI00153 - Änderung Bewertungszahl
      *
      *   Rev 2.3   Jun 30 1999 09:56:00   T13EX51
      *AKI00030 Einfügen WKZ
      *
      *   Rev 2.2   Apr 08 1999 16:42:50   T13EX51
      *AKI00021
      *
      *   Rev 2.1   Mar 24 1999 11:09:22   T13EX39
      *AKI00021 Copy mit Vert-ID BADENIA-BSP 
      *
      *   Rev 2.0   Feb 27 1999 12:58:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   12 Jan 1999 15:48:56   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************        
      *              #####################                             *        
      *     ########    D R K I 0 8 1 9    ########                    *
      *              #####################                             *        
      *                                                                *        
      *  COPY-MEMBER f?r BADENIA-Daten die in die Tabelle              *        
      *                BAUSPAREN                                       *        
      *       eingestellt werden                                       *        
      *                                                                *        
      *    SATZLŽNGE   128
081100*    SATZLŽNGE   129 - Bewertungszahl
291101*    SATZLŽNGE   135 - Vertragssumme und Fehlbetrag
100303*    SATZLÄNGE   168 - ZINSPLUS/BWZVORJAHR/ZINSLFDJAHR
      *                                                                *        
      *----------------------------------------------------------------*        
      *    LOGBUCH                                                     *        
      *DATUM   !      ŽNDERUNG                         !NAME     !VERS.*        
      *AUFTRAG !                                       !         !     *        
      *--------!---------------------------------------!---------!-----*        
      *23.03.99!NEUERSTELLUNG                          !WHO      ! 01  *
      *KI-NEU  !                                       !         !     *        
      *        !                                       !         !     *        
      *AKI00030!Einfügen Währungskennzeichen           !Dobslaff ! 02  *
      *        !                                       !         !     *        
081100*        !                                       !         !
081100*08.11.99! BEWERTUNGSZAHL um eine Vorkomma-      !P. Pohl  !
081100*AKI00153! stelle erweitert                      !         !
      *        !                                       !         !     *
291101*29.11.01! Einführung Dezimalstellen für:        !P. Pohl  !
291101*AKI00258! D-VSUMME (Vertragssumme) sowie        !         !
291101*        ! D-FEHL-MIN-ANSPAR (Fehlbetrag)        !         !
      *        !                                       !         !     *        
100303*AKI00333! neue Felder ZINSPLUS/BWZVORJAHR       !P.Hübner !     *
      *        !                ZINSLFDJAHR            !         !     *
      *        !                                       !         !     *
      ******************************************************************        
      *                                                                         
       01  DRKI0819.
      *                                                                         
           05  BSP-VERTID                      PIC  X(013).

           05  BSP-GESAMT.
               15  BSP-GES                     PIC  9(002).
               15  BSP-VNR                     PIC  9(015).
      *                                                                         
               15  BSP-KONTOSTAND              PIC  S9(002).
               15  BSP-AEND-KONTOSTAND-KZ      PIC  X(001).
      *            J WENN KONTOSTAND GEAENDERT
      *                                                                         
081100*        15  BSP-BEWERTUNGSZAHL          PIC  9(002)V9(5).
081100         15  BSP-BEWERTUNGSZAHL          PIC  9(003)V9(5).
               15  BSP-AEND-BEWERTUNGSZAHL-KZ  PIC  X(001).
      *            J WENN BEWERTUNGSZAHL GEAENDERT
      *                                                                         
               15  BSP-DARL-ZINSSATZ           PIC  9(003)V99.
               15  BSP-AEND-DARL-ZINSSATZ-KZ   PIC  X(001).
      *            J WENN DARLEHENS-ZINSSATZ GEAENDERT
      *                                                                         
               15  BSP-WOBAU-KUMUL             PIC  S9(009)V99.
               15  BSP-AEND-WOBAU-KUMUL-KZ     PIC  X(001).
      *            J WENN KUMULIERTE WOHNUNGSBAUPRAEMIE GEAENDERT
      *                                                                         
               15  BSP-DATUM-WOHNBAUPR         PIC  X(010).
               15  BSP-AEND-DATUM-WOHNBAUPR-KZ PIC  X(001).
      *            J WENN DATUM WOHNUNGSBAUPRAEMIE GEAENDERT
      *                                                                         
               15  BSP-DATUM-ZUTEILUNG         PIC  X(010).
               15  BSP-AEND-DATUM-ZUTEILUNG-KZ PIC  X(001).
      *            J WENN DATUM ZUTEILUNGSAUSSICHT GEAENDERT
      *                                             55                          
               15  BSP-DISAGIO-PROZ            PIC  9(003)V99.
               15  BSP-AEND-DISAGIO-PROZ-KZ    PIC  X(001).
      *            J WENN DISAGIO-PROZENTSATZ GEAENDERT
      *                                                                         
               15  BSP-ENDE-ST-LAUFZEIT        PIC  X(010).
               15  BSP-AEND-ENDE-ST-LAUFZ-KZ   PIC  X(001).
      *            J WENN ENDE STEUERLICHE LAUFZEIT GEAENDERT
      *                                                                         
291101*        15  BSP-FEHLBETRAG              PIC  9(007).
291101         15  BSP-FEHLBETRAG              PIC  9(008)V99.
               15  BSP-AEND-FEHLBETRAG-KZ      PIC  X(001).
      *            J WENN FEHLBETRAG ZU MINDESTANSPARUNG GEAENDERT
      *                                                                         
               15  BSP-TARIF-VAR-T3            PIC  X(001).
               15  BSP-AEND-TARIF-VAR-T3-KZ    PIC  X(001).
      *            J WENN TARIF GEAENDERT WURDE
      *                                                                         
291101*        15  BSP-VERTRAGSSUMME           PIC  9(007).
291101         15  BSP-VERTRAGSSUMME           PIC  9(008)V99.
               15  BSP-AEND-VERTRAGSSUMME-KZ   PIC  X(001).
      *            J WENN VERTRAGSSUMME GEAENDERT
      *                                                                         
               15  BSP-MINDESTANSPARUNG-PROZ   PIC  9(003)V99.
               15  BSP-AEND-MINDESTANSPAR-KZ   PIC  X(001).
      *            J WENN MINDESTANSPARUNG PROZENT GEAENDERT
      *                                                                         
               15  BSP-PRODUKT-SCHL            PIC  9(002).
               15  BSP-AEND-PRODUKT-SCHL-KZ    PIC  X(001).
      *
100303     15  BSP-ZINSPLUS                PIC  9(009)V99.
100303     15  BSP-AEND-ZINSPLUS-KZ        PIC  X(001).
100303     15  BSP-BWZ-VORJAHR             PIC  9(003)V9(005).
100303     15  BSP-AEND-BWZ-VORJAHR-KZ     PIC  X(001).
100303     15  BSP-ZINS-LFD-JAHR           PIC  S9(009)V99.
100303     15  BSP-AEND-ZINS-LFD-JAHR-KZ   PIC  X(001).
               15  BSP-WKZ                     PIC  X(001).
               15  BSP-AEND-WKZ-KZ             PIC  X(001).
      *                                                                         
               15  BSP-AKTUAL-KZ               PIC  X(001).
      *            I = INSERT
      *            U = UPDATE
      *            D = DELETE
      ******************************************************************        
