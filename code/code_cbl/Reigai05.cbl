000010*****************************************************************
000020* �q�d�h�f�`�h�O�T�F��O�}�X�^�m�q�k���U�S�O�n                  *
000021*                   �� �p�X���[�h�p                             *
000030*****************************************************************
000040* SYSYTEM NAME:�_ FOR WINNDOWS                                  *
000050* LANGUAGE PG :COBOL97 FOR WINDOWS                              *
000060* PRODUCT DATE:2001.07.04                                       *
000070* AUTHOR      :����@��                                         *
000080* PURPOSE     :�敪�R�[�h���� ��O ���Ǘ�                       *
000081***                                                             *
000082* �v���t�B�b�N�X���O�T�|�ɂ���                                *
000090*****************************************************************
000100 01  ���R�[�h.
000110     03  ���R�[�h�L�[.
000120         05  �敪�R�[�h                PIC 9(2).
000130*
000131*******************************************************************
000132* �O�O�F�敪�R�[�h�̌n                                            *
000133* �O�P�F���������\    �O�Q�F�f�@���p   �O�R�F�����s�����[�p       *
000134* �O�S�F���̑�����    �O�T�F�p�X���[�h�p                          *
000135*******************************************************************
000290*
000300         05  �ԍ��R�[�h                PIC 9(3).
000310     03  ���R�[�h�f�[�^.
000320         05  �p�X���[�h���e   OCCURS 90.
000330            07  �p�X���[�h             PIC X(6).
000340            07  ���x��                 PIC X.
000370         05  FILLER                    PIC X(5).
