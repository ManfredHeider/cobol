      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0018.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0018.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:37:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010001
      *    COPY-MEMBER: STEUERFELD 'DIALOGZUSTAND'  (DRAT0018)        * 00020001
      *                 - PROGRAMMRAHMEN + FACHFUNKTION -             * 00030001
      *    --------------------------------------------------------   * 00040001
      *    VERSION                                                    * 00050001
      *    --------------------------------------------------------   * 00060001
      *    ERSTELLT: W. SCHOELLHAMMER (INTEGRATA AG)                  * 00070001
      *    DATUM:    21.01.1993                                       * 00080001
      *    STAND:    21.01.1993                                       * 00090001
      ***************************************************************** 00100001
      *                                                                 00110001
      * ACHTUNG: DER S-DIALOG-ZUSTAND WIRD IN JEDEM PROGRAMM, DAS DEN   00120001
      *          RAHMEN VERWENDET, REDEFINIERT.                         00130001
      *          --> BEI ÄNDERUNG MUSS JEDES BESAGTE PROGRAMM ANGEPASST 00140001
      *          WERDEN !!                                              00150001
      *                                                                 00160001
      *   DIALOGZUSTAND BEI EINSTIEG/VERLASSEN DES DIALOGPROGRAMMS      00170001
       01  S-DIALOG-ZUSTAND.                                            00180000
      *       AKTUELLE BILDNUMMER                                       00190000
           15  S-BILDNR             PIC  9(02).                         00200000
      *       AKTUELLER BILDZUSTAND                                     00210000
           15  S-BILD-ZUSTAND       PIC  9(01).                         00220000
                                                                        00230001
      *                                                                 00240001
      ******** ENDE COPY-MEMBER DIALOGZUSTAND (DRAT0018) ************** 00250002
