      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drki0820.cpy  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:\MF\CPY\drki0820.cpv  $
      *
      *   Rev 2.4   Jun 30 1999 09:57:20   T13EX51
      *AKI00030 Einfügen WKZ
      *
      *   Rev 2.3   Apr 16 1999 10:01:38   T13EX51
      *AKI00021 Vorzeichen bei Guthaben
      *
      *   Rev 2.2   Apr 08 1999 16:07:40   T13EX51
      *AKI00021
      *
      *   Rev 2.1   Mar 24 1999 11:09:38   T13EX39
      *AKI00021 Copy mit Vert-ID BADENIA-GHP 
      *
      *   Rev 2.0   Feb 27 1999 12:58:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   12 Jan 1999 15:49:04   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************        
      *              #####################                             *        
      *     ########    D R K I 0 8 2 0    ########                    *
      *              #####################                             *        
      *                                                                *        
      *  COPY-MEMBER für BADENIA-Daten die in die Tabelle              *
      *              GUTHABENPRODUKT                                   *        
      *      eingestellt werden.                                       *        
      *                                                                *        
      *    SATZLÄNGE    73                                             *
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
      *        !                                       !         !     *        
      ******************************************************************        
      *                                                                         
       01  DRKI0820.

           05  GHP-VERTID                      PIC  X(013).

           05  GHP-GESAMT.

               15  GHP-GES                     PIC  9(002).
               15  GHP-VNR                     PIC  9(015).
      *                                                                         
               15  GHP-PRODUKT-ID              PIC  9(002).
               15  GHP-PRODUKT-ID-AEKZ         PIC  X(001).
      *                                                                         
               15  GHP-ANLAGEN-NAME            PIC  X(015).
               15  GHP-ANLAGEN-NAME-AEKZ       PIC  X(001).
      *                                                                         
               15  GHP-ZINSSATZ-ALLG           PIC  9(003)V99.
               15  GHP-ZINSSATZ-ALLG-AEKZ      PIC  X(001).
      *            J WENN ZINSSATZ-ALLGEMEIN GEAENDERT
      *                                                                         
      *                                                                         
               15  GHP-INFOSPERRE              PIC  X(001).
      *                                                                         
               15  GHP-VERFUEGUNG              PIC  X(002).
      *                                                                         
               15  GHP-GUTHABEN                PIC  S9(009)V99.
               15  GHP-GUTHABEN-AEKZ           PIC  X(001).
      *
               15  GHP-WKZ                     PIC  X(001).
               15  GHP-WKZ-AEKZ                PIC  X(001).
      *                                                                         
               15  GHP-AKTUAL-KZ               PIC  X(001).
      *            I = INSERT
      *            U = UPDATE
      *            D = DELETE
      ******************************************************************        
